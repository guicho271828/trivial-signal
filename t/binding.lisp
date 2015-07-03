
#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :debug *features*))

(ql:quickload :trivial-signal)

(defpackage :ts-test-binding
  (:use :cl :trivial-signal :bt))
(in-package :ts-test-binding)

;;;; prepare

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

(defun waitloop (i)
  ;;(lprint trivial-signal::*signal-handler-hierarchy*)
  (loop (sleep 1)
        (format t "waiting signal ~a (step ~a) on ~a" *signo* i (bt:current-thread))))

(defvar *runner*)

;;;; macro version

(defmacro %run1 ()
  `(tagbody
     (signal-handler-bind
         ((,*signo* (lambda (c) (lprint :first)))
          (,*signo* (lambda (c) (lprint :escaping) (go :escape)))
          (,*signo* (lambda (c) (lprint :this-should-not-be-called))))
       (waitloop 1))
     :escape
     (lprint :success1)))
(defmacro %run2 ()
  `(tagbody
     (signal-handler-bind
         ((,*signo* (lambda (c)  (lprint :escaping) (go :escape))))
       (waitloop 2))
     :escape
     (lprint :success2)))
(defmacro %run3 ()
  `(tagbody
     (signal-handler-bind ((,*signo* (lambda (c) (lprint :should-not-be-called))))
       (signal-handler-bind ((,*signo* (lambda (c) (lprint :escaping) (go :escape))))
         (signal-handler-bind ((,*signo* (lambda (c) (lprint :inner))))
           (waitloop 3))))
     :escape
     (lprint :success3)))

;;;; dynamic version

(defun run1 ()
  (tagbody
    (call-signal-handler-bind
     `((,*signo* ,(lambda (c) (lprint :first))
                 ,(lambda (c) (lprint :escaping) (go :escape))
                 ,(lambda (c) (lprint :this-should-not-be-called))))
     (lambda () (waitloop 4)))
    :escape
    (lprint :success4)))
(defun run2 ()
  (tagbody
    (call-signal-handler-bind
     `((,*signo* ,(lambda (c)  (lprint :escaping) (go :escape))))
     (lambda () (waitloop 5)))
    :escape
    (lprint :success5)))
(defun run3 ()
  (tagbody
    (call-signal-handler-bind
     `((,*signo* ,(lambda (c) (lprint :should-not-be-called))))
     (lambda ()
       (call-signal-handler-bind
        `((,*signo* ,(lambda (c) (lprint :escaping) (go :escape))))
        (lambda ()
          (call-signal-handler-bind
           `((,*signo* ,(lambda (c) (lprint :inner))))
           (lambda () (waitloop 6)))))))
    :escape
    (lprint :success6)))

;;;; run

(format t "~&Ready")

(setf *runner* (make-thread
                (lambda ()
                  (lprint "testing static version")
                  (%run1) (%run2) (%run3)
                  (run1) (run2) (run3))))

(defvar *killer*
    (make-thread (lambda ()
                   (loop
                     (sleep 3)
                     (format *shared* "~&sending signal ~a to pid ~a"
                             *signo* *pid*)
                     (force-output *shared*)
                     #+sbcl (sb-posix:kill *pid* *signo*)
                     #+ccl (ccl:run-program "kill" (list (format nil "-~a" *signo*)
                                                         (princ-to-string *pid*)))))
                 :name "killer")
  "a thread that periodically send *singo* to *pid*")


(bt:join-thread *runner*)
