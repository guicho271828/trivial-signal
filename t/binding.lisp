
(pushnew :debug *features*)
(ql:quickload :trivial-signal)

(defpackage :ts-test-binding
  (:use :cl :trivial-signal :bt))
(in-package :ts-test-binding)

(format t "ready~%")

;; (handler-bind ((error (lambda (c) (print :hi!)))
;;                (error (lambda (c) (print :hi!))))
;;   (error "cruel!"))

(defvar *pid* (sb-posix:GETPID))
(defvar *signo* 30)
(print *pid*) (force-output)
(defvar *shared* *standard-output*)
(defmacro run ()
  `(block run
     (tagbody
       (signal-handler-bind
           ((,*signo* (lambda (c) (print :first)))
            (,*signo* (lambda (c) (go :escape)))
            (,*signo* (lambda (c) (print :this-should-not-be-called))))
         (loop (sleep 1) (print :hi!)
               (print trivial-signal::*signal-handler-hierarchy*)))
       :escape
       (print :success))
     (tagbody
       (signal-handler-bind ((,*signo* (lambda (c) (print :this-should-not-be-called))))
         (signal-handler-bind ((,*signo* (lambda (c) (go :escape))))
           (signal-handler-bind ((,*signo* (lambda (c) (print :inner))))
             (loop (sleep 1) (print :hi!)
                   (print trivial-signal::*signal-handler-hierarchy*)))))
       :escape
       (print :success))))


;; (defvar *runner*
;;     (make-thread #'run :name "runner"))


(defvar *killer*
    (make-thread (lambda ()
                   (loop
                     (handler-bind ((trivial-signal::unix-signal
                                     (lambda (c)
                                       (print :killer-thread-ignore)
                                       (invoke-restart
                                        (find-restart 'ignore)))))
                       (sleep 4)
                       (format *shared* "~&sending signal ~a to pid ~a"
                               *signo* *pid*)
                       (force-output *shared*)
                       (sb-posix:KILL *pid* *signo*))))
                 :name "killer"))

(run)




