(in-package :cl-user)
(defpackage trivial-signal
  (:use :cl
        :trivial-signal.signals)
  (:import-from :cffi
                :defcallback
                :foreign-funcall
                :callback)
  (:export :with-signal-handler
           :signal-handler-bind
           :*toplevel-signal-handlers*

           :unix-signal
           :signo

           :signal-name
           :signal-number
           :+sighup+
           :+sigint+
           :+sigquit+
           :+sigill+
           :+sigtrap+
           :+sigabrt+
           :+sigemt+
           :+sigfpe+
           :+sigkill+
           :+sigbus+
           :+sigsegv+
           :+sigsys+
           :+sigpipe+
           :+sigalrm+
           :+sigterm+
           :+sigurg+
           :+sigstop+
           :+sigtstp+
           :+sigcont+
           :+sigchld+
           :+sigttin+
           :+sigttou+
           :+sigio+
           :+sigxcpu+
           :+sigxfsz+
           :+sigvtalrm+
           :+sigprof+
           :+sigwinch+
           :+siginfo+
           :+sigusr1+
           :+sigusr2+))
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

(defun canonical-signal-arg (signal)
  (if (integerp signal)
      signal
      (signal-number signal)))

;; you no longer able to explicitly access the current handlers.

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
(defvar *print-lock* (bt:make-lock "print-lock"))

(define-condition unix-signal ()
  ((signo :initarg :signo :reader signo))
  (:report (lambda (c s)
             (format s "~&received ~A~%" (signal-name (signo c))))))

;; utility
(defun assocdr (item alist) (cdr (assoc item alist)))
(cffi:defcallback call-signal-handler-in-lisp :void ((signo :int))
  #+debug
  (bt:with-lock-held (*print-lock*)
    (format t "~&signo: ~a" signo)
    (format t "~&all threads: ~a" (bt:all-threads))
    (format t "~&current thread: ~a" (bt:current-thread))
    (force-output))
  (dolist (th (bt:all-threads))
    (bt:interrupt-thread th #'invoke-handlers signo)))

(defun invoke-handlers (signo)
  "Handler-invoking procedure per thread"
  #+debug
  (bt:with-lock-held (*print-lock*)
    (format t "~&-------- handler invoked in thread: ~a -------"
            (bt:thread-name (bt:current-thread)))
    (format t "~&hierarchy: ~a" *signal-handler-hierarchy*)
    (force-output))
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
      :report "ignore this unix signal.")))

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

;; TODO : sbcl's handler-bind turns (lambda ... ) in the handler definition
;; into locally-defined, dynamic-extent function (for optimization).

(defmacro signal-handler-bind (bindings &body forms)
  "Execute FORMS in a dynamic environment where signal handler bindings are in effect."
  (let ((next-enabled-signals (gensym))
        (new-signal-handlers (gensym))
        (new-hierarchy (gensym)))
    ;; we need a special care in these codes because it handles
    ;; asynchronous operations
    `(let* ((,new-signal-handlers ,(%inline-bindings bindings))
            (,next-enabled-signals (%next-enabled-signals ,new-signal-handlers))
            (,new-hierarchy (cons ,new-signal-handlers *signal-handler-hierarchy*))
            (*currently-enabled-signals*
             (append ,next-enabled-signals
                     *currently-enabled-signals*))
            ;; ^^^ this is safe because the only function dependent on
            ;; *currently-enabled-signals* is %next-enabled-signals.
            (*signal-handler-hierarchy* ,new-hierarchy))
       ;; <---- what happens if an interrupt for a signal X occurs right here?
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
             ;; Only the additional signals are enabled.
             ;; It is possible that a new signal is received
             ;; while partly enabling these signals before running ,@forms ...
             (%enable-all-signal-handlers ,next-enabled-signals)
             ,@forms)
         ;; however in any cases, the partly/fully enabled singals are
         ;; correctly disabled.
         (%disable-all-signal-handlers
          ,next-enabled-signals)))))

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


