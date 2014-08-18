

(ql:quickload :trivial-signal)
(use-package :trivial-signal)

(setf (signal-handler (read-from-string (second sb-ext:*posix-argv*)))
      (lambda (signo)
        (format t "trivial-signal: ~a~%" (signal-name signo))
        (sb-ext:exit)))

(format t "ready~%")

(loop)

