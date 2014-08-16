(in-package :cl-user)
(defpackage trivial-signal
  (:use :cl
        :trivial-signal.signals)
  (:import-from :cffi
                :defcallback
                :foreign-funcall
                :callback)
  (:export :signal-handler
           :remove-signal-handler
           :remove-all-signal-handlers
           :with-signal-handler
           :signal-handler-bind

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

(defvar *signal-handlers* (make-hash-table :test 'eql))

(defun canonical-signal-arg (signal)
  (if (integerp signal)
      signal
      (signal-number signal)))

(defun signal-handler (signal)
  "Return a signal handler for a signal SIGNAL."
  (gethash (canonical-signal-arg signal) *signal-handlers*))

(defun (setf signal-handler) (fn signal)
  "Set a signal handler FN for a signal SIGNAL."
  (check-type fn (or function symbol))
  (let ((signo (canonical-signal-arg signal)))
    (enable-signal-handler signo)
    (setf (gethash signo *signal-handlers*) fn)))

(defun remove-signal-handler (signal)
  "Remove a signal handler FN from a signal SIGNAL."
  (disable-signal-handler (canonical-signal-arg signal)))

(defun remove-all-signal-handlers ()
  "Clear all signal handlers."
  (maphash (lambda (signo fn)
             (declare (ignore fn))
             (disable-signal-handler signo))
           *signal-handlers*)
  (setf *signal-handlers* (make-hash-table :test 'eql))
  (values))

;; FIXME: according to the man page of SIGNAL(2):
;; ```The  behavior of signal() varies across UNIX versions, and has also
;; varied historically across different versions of Linux.   Avoid  its
;; use: use sigaction(2) instead.  See Portability below.'''

(cffi:defcallback signal-handler :void ((signo :int))
  (multiple-value-bind (fn foundp)
      (gethash signo *signal-handlers*)
    (when foundp
      (funcall fn signo))))

(defun enable-signal-handler (signo)
  (unless (nth-value 1 (gethash signo *signal-handlers*))
    (cffi:foreign-funcall "signal" :int signo :pointer (cffi:callback signal-handler))))

(defun disable-signal-handler (signo)
  (when (nth-value 1 (gethash signo *signal-handlers*))
    (remhash signo *signal-handlers*)
    (cffi:foreign-funcall "signal" :int signo :unsigned-long 0)))

(defmacro with-signal-handler (signal fn &body forms)
  "Execute FORMS in an environment where a signal handler FN for a signal SIGNAL is in effect."
  (let ((original (gensym "ORIGINAL"))
        (foundp (gensym "FOUNDP"))
        (g-signal (gensym "SIGNAL")))
    `(let* ((,g-signal ,signal))
       (multiple-value-bind (,original ,foundp) (signal-handler ,g-signal)
         (setf (signal-handler ,g-signal) ,fn)
         (unwind-protect (progn ,@forms)
           (if ,foundp
               (setf (signal-handler ,g-signal) ,original)
               (remove-signal-handler ,g-signal)))))))

(defmacro signal-handler-bind (bindings &body forms)
  "Execute FORMS in an environment where signal handler bindings are in effect."
  (if bindings
      `(with-signal-handler ,@(car bindings)
         (signal-handler-bind ,(cdr bindings) ,@forms))
      `(progn ,@forms)))
