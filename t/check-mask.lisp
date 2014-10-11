
#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :debug *features*))

;; (asdf:compile-system (asdf:find-system :trivial-signal))
(ql:quickload :trivial-signal)
(ql:quickload :iterate)

(defpackage :ts-test-binding
  (:use :cl :trivial-signal :bt :iterate))
(in-package :ts-test-binding)

(format t "~&Preparing...")
(defvar *shared* *standard-output*)
(defvar *print-lock* (bt:make-lock "print-test"))
(defun lprint (thing)
  (print thing *shared*))

(defvar *pid*
    #+sbcl (sb-posix:getpid)
    #+ccl (ccl::getpid))
(defvar *signo* 29)
(format t "~&main thread pid: ~a" *pid*)

#+sbcl
(sb-ext:disable-debugger)

(defvar *lock* (bt:make-lock))
(bt:acquire-lock *lock*)
(defvar *killer*
    (make-thread (lambda ()
                   (loop
                     (sleep 1)
                     (bt:with-lock-held (*lock*)
                       (format *shared* "~&sending signal ~a to pid ~a"
                               *signo* *pid*)
                       (force-output *shared*)
                       #+sbcl (sb-posix:kill *pid* *signo*)
                       #+ccl (ccl:run-program "kill" (list (format nil "-~a" *signo*)
                                                           (princ-to-string *pid*))))))
                 :name "killer")
  "a thread that periodically send *singo* to *pid*")

(format t "~&Ready")

(print #+ccl ccl:*unprocessed-command-line-arguments*
       #+sbcl sb-ext:*posix-argv*)

(setf *signo*
      (read-from-string
       #+ccl (first ccl:*unprocessed-command-line-arguments*)
       #+sbcl (third sb-ext:*posix-argv*)))

(defun exit (&optional (status 0))
  #+ccl (ccl:quit status)
  ;; http://www.sbcl.org/manual/#Exit
  #+sbcl (sb-ext:exit :code status)
  ;; http://franz.com/support/documentation/9.0/doc/operators/excl/exit.htm
  #+allegro (excl:exit status :quiet t)
  #+clisp (ext:quit status)
  #+cmucl (unix:unix-exit status)
  #+ecl (ext:quit status)
  #-(or ccl sbcl allegro clisp cmucl ecl) (cl-user::quit))

(call-signal-handler-bind
 `((,*signo* ,(lambda (c) (exit 0))))
 (lambda ()
   (format t "~&listener thread ready, waiting ~a" *signo*)
   (bt:release-lock *lock*)
   (sleep 1)
   (bt:acquire-lock *lock*)
   (exit 1)))


