
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :debug *features*))
(ql:quickload :trivial-signal)

(defpackage :ts-test-binding
  (:use :cl :trivial-signal :bt))
(in-package :ts-test-binding)

(format t "ready~%")

;; (handler-bind ((error (lambda (c) (print :hi!)))
;;                (error (lambda (c) (print :hi!))))
;;   (error "cruel!"))

(defvar *shared* *standard-output*)
(defvar *print-lock* (bt:make-lock "print-test"))
(defun lprint (thing)
  #+nil
  (bt:with-lock-held (*print-lock*)
    (print thing *shared*))
  (print thing *shared*))

(defvar *pid*
    #+sbcl (sb-posix:getpid)
    #+ccl (ccl::getpid))
(defvar *signo* 30)
(lprint *pid*) (force-output)

(defmacro %run1 ()
  `(tagbody
     (signal-handler-bind
         ((,*signo* (lambda (c) (lprint :first)))
          (,*signo* (lambda (c) (lprint :escaping) (go :escape)))
          (,*signo* (lambda (c) (lprint :this-should-not-be-called))))
       (lprint trivial-signal::*signal-handler-hierarchy*)
       (loop (sleep 1)
             (lprint "waiting signal (step 1)")))
     :escape
     (lprint :success)))
(defmacro %run2 ()
  `(tagbody
     (signal-handler-bind
         ((,*signo* (lambda (c)  (lprint :escaping) (go :escape))))
       
       (loop (sleep 1)
             (lprint trivial-signal::*signal-handler-hierarchy*)
             (lprint "waiting signal (step 2)")))
     :escape
     (lprint :success)))
(defmacro %run3 ()
  `(tagbody
     (signal-handler-bind ((,*signo* (lambda (c) (lprint :should-not-be-called))))
       (signal-handler-bind ((,*signo* (lambda (c) (lprint :escaping) (go :escape))))
         (signal-handler-bind ((,*signo* (lambda (c) (lprint :inner))))
           (lprint trivial-signal::*signal-handler-hierarchy*)
           (loop (sleep 1)
                 (lprint "waiting signal (step 3)")
                 (lprint (bt:current-thread))))))
     :escape
     (lprint :success)))

(defvar *killer*
    (make-thread (lambda ()
                   (loop
                     (handler-bind ((trivial-signal::unix-signal
                                     (lambda (c)
                                       (lprint :killer-thread-ignore)
                                       (lprint (find-restart 'ignore))
                                       (invoke-restart
                                        (find-restart 'ignore)))))
                       (sleep 3)
                       (format *shared* "~&sending signal ~a to pid ~a"
                               *signo* *pid*)
                       (force-output *shared*)
                       #+sbcl (sb-posix:kill *pid* *signo*)
                       #+ccl (ccl:run-program "kill" (list (format nil "-~a" *signo*)
                                                           (princ-to-string *pid*))))))
                 :name "killer"))

(defun run1 () (%run1))
(defun run2 () (%run2))
(defun run3 () (%run3))
(defun run () (run1) (run2) (run3))

(defvar *runner*
    (make-thread #'run :name "runner"))

(bt:join-thread *runner*)
;; (dolist (*signo* '(2 15 24 30)) ; int, term, xcpu, usr1
;;   (eval '(run)))




;; (tagbody
;;   (make-thread (lambda () (go :escape)))
;;   :escape
;;   (print :hi!))
