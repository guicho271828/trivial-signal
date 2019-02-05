
;; this file shows the slight difference between sbcl and ccl
;; in handling SIGINT signals

(defun run ()
  (tagbody
    (handler-bind ((condition
                    (lambda (c)
                      (declare (ignore c))
                      (go :escape))))
      (restart-bind ((my-continue
                      (lambda ()
                        (go :escape))))
        (loop
          (sleep 1)
          (print :waiting)
          (terpri)
          (force-output))))
    :escape
    (print :finished)))

(run)

;; press C-c to send SIGINT


;; -------------------
;; invoking from bash

;; [guicho t]$ sbcl --load interrupt.lisp
;; This is SBCL 1.2.1, an implementation of ANSI Common Lisp.
;; More information about SBCL is available at <http://www.sbcl.org/>.

;; SBCL is free software, provided as is, with absolutely no warranty.
;; It is mostly in the public domain; some portions are provided under
;; BSD-style licenses.  See the CREDITS and COPYING files in the
;; distribution for more information.

;; loading Quicklisp...

;; :WAITING 

;; :WAITING 
;;   C-c C-c
;; :FINISHED 
;; * [guicho t]$ 

;; [guicho t]$ ccl -b --load interrupt.lisp

;; loading Quicklisp...

;; :WAITING 

;; :WAITING 

;; :WAITING 

;; :WAITING 
;;   C-c C-c> Break: interrupt signal
;; > While executing: CCL::%NANOSLEEP, in process listener(1).
;; > Type :GO to continue, :POP to abort, :R for a list of available restarts.
;; > If continued: Return from BREAK.
;; > Type :? for other options.
;; 1 > :r
;; >   Type (:C <n>) to invoke one of the following restarts:
;; 0. Return to break level 1.
;; 1. #<RESTART ABORT-BREAK #x7F506EECCA2D>
;; 2. Return from BREAK.
;; 3. #<RESTART MY-CONTINUE #x7F506EECCF0D>
;; 4. Retry loading "interrupt.lisp"
;; 5. Skip loading "interrupt.lisp"
;; 6. Load other file instead of "interrupt.lisp"
;; 7. Skip loading "interrupt.lisp"
;; 8. Abort startup.
;; 9. Reset this thread
;; 10. Kill this thread
;; 1 > 3

;; :FINISHED 
;; Welcome to Clozure Common Lisp Version 1.9-r15757  (LinuxX8664)!
;; ? 
;; Invoking restart: #<RESTART MY-CONTINUE #x7F506EECCF0D>
;; [guicho t]$ 
