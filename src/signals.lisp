
(in-package :trivial-signal.signals)

(defvar *signal-keyword* (make-hash-table))

(defun constant->keyword (symbol)
  "+SIGHUP+ -> :HUP "
  (let ((name (symbol-name symbol)))
    (intern (subseq name 4 (1- (length name))) :keyword)))

(defun constant->sigkeyword (symbol)
  "+SIGHUP+ -> :SIGHUP "
  (let ((name (symbol-name symbol)))
    (intern (subseq name 1 (1- (length name))) :keyword)))


(defun keyword->constant (symbol)
  ":HUP -> +SIGHUP+"
  (let ((p (find-package :trivial-signal.signals)))
    (or
     ;; :sighup
     (find-symbol (format nil "+~a+" (symbol-name symbol)) p)
     ;; :hup
     (find-symbol (format nil "+SIG~a+" (symbol-name symbol)) p))))

(dolist (symbol '(
                  ;; 1-1990
                  +SIGHUP+ +SIGINT+ +SIGQUIT+ +SIGILL+ +SIGABRT+ +SIGFPE+ +SIGKILL+ +SIGSEGV+
                  +SIGPIPE+ +SIGALRM+ +SIGTERM+ +SIGUSR1+ +SIGUSR2+ +SIGCONT+ +SIGCHLD+
                  +SIGSTOP+ +SIGTSTP+ +SIGTTIN+ +SIGTTOU+
                  ;; 1-2001
                  +SIGBUS+ +SIGPOLL+ +SIGPROF+
                  +SIGSYS+ +SIGTRAP+ +SIGURG+ +SIGVTALRM+ +SIGXCPU+ +SIGXFSZ+
                  ;; non-posix
                  +SIGIOT+
                  +SIGEMT+ +SIGTKFLT+ +SIGIO+ +SIGCLD+ +SIGPWR+ +SIGINFO+ +SIGLOST+
                  +SIGWINCH+ +SIGUNUSED+
                  ;; realtime
                  +SIGRTMIN+
                  ;; +SIGRTMIN+1+ +SIGRTMIN+2+ +SIGRTMIN+3+
                  ;; +SIGRTMIN+4+ +SIGRTMIN+5+ +SIGRTMIN+6+ +SIGRTMIN+7+ +SIGRTMIN+8+
                  ;; +SIGRTMIN+9+ +SIGRTMIN+10+ +SIGRTMIN+11+ +SIGRTMIN+12+ +SIGRTMIN+13+
                  ;; +SIGRTMIN+14+ +SIGRTMIN+15+ +SIGRTMAX-14+ +SIGRTMAX-13+ +SIGRTMAX-12+
                  ;; +SIGRTMAX-11+ +SIGRTMAX-10+ +SIGRTMAX-9+ +SIGRTMAX-8+ +SIGRTMAX-7+
                  ;; +SIGRTMAX-6+ +SIGRTMAX-5+ +SIGRTMAX-4+ +SIGRTMAX-3+ +SIGRTMAX-2+
                  ;; +SIGRTMAX-1+
                  +SIGRTMAX+
                  ;; bsd
                  +SIGTHR+ +SIGLIBRT+))
  (handler-case
      (setf (gethash (symbol-value symbol) *signal-keyword*)
            (constant->keyword symbol))
    (unbound-variable ())))

(defun signal-name (signo)
  "Return the name of SIGNO as a keyword."
  (gethash signo *signal-keyword*))

(defun signal-number (sigspec)
  "Return the integer of SIGSPEC, which follows the sigspec API e.g. +SIGHUP+, :hup or :sighup."
  (symbol-value (keyword->constant sigspec)))
