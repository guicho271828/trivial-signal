(in-package :cl-user)
(defpackage trivial-signal-asd
  (:use :cl :asdf))
(in-package :trivial-signal-asd)

(cl:eval-when (:load-toplevel :execute)
  #+quicklisp
  (ql:quickload :cffi-grovel :silent t))

(defsystem trivial-signal
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "Public Domain"
  :depends-on (:cffi :bordeaux-threads)
  :defsystem-depends-on (:cffi-grovel)
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:cffi-grovel-file :grovel)
                 (:file "signals")
                 (:file "trivial-signal"))
                :serial t))
  :description "Unix signal handling library."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op trivial-signal-test))))
