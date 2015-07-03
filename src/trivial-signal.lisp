
(in-package :trivial-signal)

;;;; hierarchical handler binding structure

#|

in ANSI, handler-bind should allow multiple handlers for the same
condition:

 (handler-bind ((error (lambda (c) (print :hi!)))
                (error (lambda (c) (print :hi2!))))
   (error "cruel!"))

and it also allows nested bindings:

     (flet ((h1 (lambda (c)
                  (declare (ignore c))
                  (when (= x 1) (go :one))))
            (h2 (lambda (c)
                  (declare (ignore c))
                  (when (= x 2) (go :two)))))
       (handler-bind ((error #'h1))
         (handler-bind ((error #'h2))
           (error "error!"))))

then the type of hierarchycal binding structure is

list (sorted-hash signo (list functions))
                         ^^^ for the multiple handlers in the same layer
^^^for hierarchy

Given signo. is always a fixnum, sorted hash can be a fixed-length vector
or sparce array. But in most cases, alist would suffice (in contrast to
the hash-table, which is used in the old version, it consumes less memory.)

Note that in most cases the number of handlers in one signal-handler-bind would be <10.
Also note that in SBCL +min-hash-threashold+ is 16.
Therefore the current implementation of hierarchycal handlers follows the type below:

list (alist signo (list functions)) == list (list (cons signo (list functions)))

|#


(defvar *toplevel-signal-handlers* nil "Toplevel handler")
(defvar *signal-handler-hierarchy* nil "Nested cons-tree of signal handlers")

;; utility
(defun assocdr (item alist) (cdr (assoc item alist)))

(defun canonical-signal-arg (signal)
  (if (integerp signal)
      signal
      (signal-number signal)))

(defun signal-handler (signal)
  "Returns the toplevel signal handler for a signal SIGNAL.
Toplevel handlers are system wide and (in most cases) static.
Unlike normal signal handlers, toplevel handlers can hold at most one
handler for the same signal.

Example:

 (use-package :trivial-signal)
 (defun exit-on-signal (signo)
   (format *error-output* \"~&received ~A~%\" (signal-name signo))
   (sb-ext:exit :code 1 :abort t))
 (setf (signal-handler :term) #'exit-on-signal) ;; :term can also be :sigterm or 15

 (loop (sleep 3)) "
  (first (assocdr (canonical-signal-arg signal)
                  *toplevel-signal-handlers*)))

(defun (setf signal-handler) (fn signal)
  "Set the toplevel signal handler FN for a signal SIGNAL.
Toplevel handlers can hold at most one handler for the same signal."
  (check-type fn (or function symbol)) ; nil is a symbol
  (let* ((signo (canonical-signal-arg signal))
         (cons (assoc (canonical-signal-arg signal)
                      *toplevel-signal-handlers*)))
    (if cons
        (setf (cdr cons) (list fn)) ; same interface as *signal-handler-hierarchy*
        (push (list signo fn) *toplevel-signal-handlers*))))

(defun remove-signal-handler (signal)
  "Deprecated.
To remove the toplevel signal handler, (setf (signal-handler signo) nil) ."
  (setf (signal-handler signal) nil))

(defun remove-all-signal-handlers ()
  "Removes all toplevel signal handlers"
  (setf *toplevel-signal-handlers* nil))

;;;; cffi interfaces

;; FIXME: according to the man page of SIGNAL(2):
;; ```The  behavior of signal() varies across UNIX versions, and has also
;; varied historically across different versions of Linux.   Avoid  its
;; use: use sigaction(2) instead.  See Portability below.'''

;; FIXME/NOTSURE: some implimentations mask / use some signals
;; internally. Therefore, binding some of them may cause the implimentation
;; to malfunction.  (6. Signal handling,
;; http://www.sbcl.org/sbcl-internals/index.html)

#+debug
(defvar *handler-lock* (bt:make-lock "handler-lock")
  "force the handler processing to be synchronous")

(define-condition unix-signal ()
  ((signo :initarg :signo :reader signo))
  (:report (lambda (c s)
             (format s "~&received ~A~%" (signal-name (signo c))))))

(defvar *listener-threads* (make-hash-table))
(defvar *listener-threads-lock* (bt:make-lock "listener-threads"))

(cffi:defcallback call-signal-handler-in-lisp :void ((signo :int))
  ;; in SBCL, singals are sent only to the main thread.
  ;; in CCL, sometimes signals are also sent to the other threads (subprocesses)
  ;; (in which case call-signal-handler-in-lisp is called multiple times.)
  ;; (Is this because CCL uses PTHREAD_SIGNAL internally?)
  (when
      #+(or sbcl ccl)
      (string=
       #+sbcl "main thread"
       #+ccl "Initial"
       (bt:thread-name (bt:current-thread)))
      #-(or sbcl ccl)
      t
    #+debug
    (bt:with-lock-held (*handler-lock*)
      (format t "~&received signo: ~a" signo)
      (format t "~&all threads: ~a" (bt:all-threads))
      (format t "~&current thread: ~a" (bt:current-thread))
      (force-output))
    (dolist (th (bt:all-threads)) ;; only the live threads
      #+debug
      (bt:with-lock-held (*handler-lock*)
        (format t "~&th: ~a result: ~a"
                th (gethash th *listener-threads*))
        #+ccl
        (print (ccl::lisp-thread.interrupt-functions th)))
      (when (gethash th *listener-threads*)
        (bt:interrupt-thread th #'invoke-handlers signo)))))

(defun invoke-handlers (signo)
  "Handler-invoking procedure per thread"
  (;; #+debug bt:with-lock-held #+debug (*handler-lock*)
   ;; #-debug
   progn
    (format t "~&-------- handler invoked in thread: ~a -------"
            (bt:thread-name (bt:current-thread)))
    (format t "~&hierarchy: ~a" *signal-handler-hierarchy*)
    (force-output)
    (dolist (handlers/layer *signal-handler-hierarchy*)
      (let ((handlers/signo (assocdr signo handlers/layer)))
        (when handlers/signo
          (dolist (handler handlers/signo)
            (funcall handler signo)))))
    ;; if they all declines, call the toplevel handlers
    (let ((handlers/signo (assocdr signo *toplevel-signal-handlers*)))
      (when handlers/signo
        (dolist (handler handlers/signo)
          (funcall handler signo))))
    ;; if all handlers have declined, then invoke the debugger
    (restart-case
        (error 'unix-signal :signo signo)
      (ignore ()
        :report "ignore this unix signal."))))

(defun %enable-signal-handler (signo)
  (check-type signo integer)
  (cffi:foreign-funcall "signal" :int signo :pointer (cffi:callback call-signal-handler-in-lisp)))

(defun %disable-signal-handler (signo)
  (check-type signo integer)
  (cffi:foreign-funcall "signal" :int signo :unsigned-long 0))

(defun %enable-all-signal-handlers (signals)
  (mapc #'%enable-signal-handler signals))
(defun %disable-all-signal-handlers (signals)
  (mapc #'%disable-signal-handler signals))

(defvar *currently-enabled-signals* nil)
(defun %next-enabled-signals (new-signal-handlers)
  (set-difference
   (mapcar #'car new-signal-handlers)
   *currently-enabled-signals*))


;;;; macros

;; TODO : sbcl's cl:handler-bind turns (lambda ... ) in the handler definition
;; into locally-defined, dynamic-extent function (for optimization).

(defvar *listening-signal-p* nil
  "per-thread flag for whether the current thread is listening signals")

(defun call-signal-handler-bind (new-signal-handlers fn)
  "Execute FN in a dynamic environment where signal handler bindings are
in effect. new-signal-handlers is a cons tree of ((signo handler ...) ...)

note that, trivial-signal only considers the first appearance of (signo handlers...)
with matching signo in the same layer. For example,

    (call-signal-handler-bind
     `((,*signo* ,(lambda (c) (lprint :first))
                 ,(lambda (c) (lprint :escaping) (go :escape))
                 ,(lambda (c) (lprint :this-should-not-be-called))))
     (lambda () ...))

is okay but

    (call-signal-handler-bind
     `((,*signo* ,(lambda (c) (lprint :first)))
       (,*signo* ,(lambda (c) (lprint :escaping) (go :escape)))
       (,*signo* ,(lambda (c) (lprint :this-should-not-be-called))))
     (lambda () ... ))

is incorrect (2nd and 3rd handlers are ignored).
If you want to do it wrap the main code in (lambda () ...)
 with another call-signal-handler-bind.
"
  (let* ((next-enabled-signals (%next-enabled-signals new-signal-handlers))
         (new-hierarchy (cons new-signal-handlers *signal-handler-hierarchy*))
         (*currently-enabled-signals*
          (append next-enabled-signals *currently-enabled-signals*))
         ;; ^^^ this is safe because the only function dependent on
         ;; *currently-enabled-signals* is %next-enabled-signals.
         (*signal-handler-hierarchy* new-hierarchy))
    ;; <-- what happens if an interrupt for a signal X occurs right here?
    ;;   (== the new hierarchy is set,
    ;;       but the newest handlers are not enabled yet)
    ;; 
    ;; old layers | new layer | behavior
    ;; -----------+-----------+--------
    ;; disabled   | enabled   | interrupts are ignored : correct
    ;; enabled    | enabled   | new handlers are in effect : correct
    ;;            |           | because they are already enabled
    (unwind-protect
        (progn
          (unless *listening-signal-p* ;; called only on the toplevel stack
            #+debug
            (bt:with-lock-held (*handler-lock*)
              (format t "~&Registering the current thread as a listener: ~a"
                      (bt:current-thread))
              (force-output))
            (bt:with-lock-held (*listener-threads-lock*)
              (setf (gethash (bt:current-thread) *listener-threads*) t)))
          ;; Only the additional signals are enabled.  It is possible
          ;; that a new signal is received while partly enabling these
          ;; signals before running ,@forms ...
          #+debug
          (bt:with-lock-held (*handler-lock*)
            (format t "~&Enabling signal handlers: ~a"
                    (bt:current-thread)))
          (%enable-all-signal-handlers next-enabled-signals)
          (let* ((*listening-signal-p* t))
            #+ccl
            (ccl:with-interrupts-enabled
              (funcall fn))
            #+sbcl
            (funcall fn)))
      ;; however in any cases, the partly/fully enabled singals are
      ;; correctly disabled.
      #+debug
      (bt:with-lock-held (*handler-lock*)
        (format t "~&Disabling signal handlers: ~a" (bt:current-thread)))
      (%disable-all-signal-handlers next-enabled-signals)
      (unless *listening-signal-p* ;; called only on the toplevel stack
        #+debug
        (bt:with-lock-held (*handler-lock*)
          (format t "~&Removng the current thread as a listener: ~a"
                  (bt:current-thread)))
        (bt:with-lock-held (*listener-threads-lock*)
          (remhash (bt:current-thread) *listener-threads*))))))

(defmacro signal-handler-bind (bindings &body forms)
  "Execute FORMS in a dynamic environment where thread-local signal handler bindings are
in effect.

The syntax is almost identical to cl:handler-bind. Example:

 (tagbody
   (signal-handler-bind ((15 (lambda (c) (print :first)))
                         (15 (lambda (c) (print :escaping) (go :escape)))
                         (2  (lambda (c) (print :escaping) (go :escape)))
                         (15 (lambda (c) (print :this-should-not-be-printed))))
     (loop (sleep 3)))
   :escape
   (print :success!))

Now send signal 15 to the main lisp process using the terminal. It should
print :FIRST, :ESCAPING and :SUCCESS.

 (Note: it does not work on some implementations, due to their
 internals. In such cases, try another signal number, e.g. 10 !)

"
  `(call-signal-handler-bind
    ,(%inline-bindings bindings)
    (lambda () ,@forms)))

(defun group-bindings (bindings)
  "returns an alist of ((name handler1 handler2 ...)=cons ...)"
  (let (res)
    (dolist (pair bindings res)
      (destructuring-bind (name function-form) pair
        (let* ((signo (canonical-signal-arg name))
               (cons (assoc signo res)))
          (if cons
              (push function-form (cdr cons)) ;; note that functions are in reverse
              (push (list signo function-form) res)))))))

(defun %inline-bindings (bindings)
  `(list 
    ,@(mapcar (lambda (cons)
                (destructuring-bind (name . handlers-rev) cons
                  `(list ,name ,@(nreverse handlers-rev))))
              (group-bindings bindings))))

(defmacro with-signal-handler (signal fn &body forms)
  "This is only for the backward compatibility"
  `(signal-handler-bind ((,signal ,fn)) ,@forms))


