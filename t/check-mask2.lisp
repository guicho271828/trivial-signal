
#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :debug *features*))

(ql:quickload '(:trivial-signal :iterate) :silent t)

(defpackage :ts-test-binding
  (:use :cl :trivial-signal :bt :iterate))
(in-package :ts-test-binding)

(defvar *pid*
    #+sbcl (sb-posix:getpid)
    #+ccl (ccl::getpid))

(defvar *signo*
    (read-from-string
     (first uiop:*command-line-arguments*)))

(call-signal-handler-bind
 `((,*signo* ,(lambda (c)
                (declare (ignore c))
                (format t "~%Success! IMPL: ~a SIGNAL: ~a NAME: ~a"
                        (lisp-implementation-type) *signo* (signal-name *signo*))
                (finish-output *standard-output*) 
                (uiop:quit 0))))
   (lambda ()
     (loop (sleep 1.5))))

