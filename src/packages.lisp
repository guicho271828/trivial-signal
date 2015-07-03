(in-package :cl-user)

(defpackage trivial-signal.signals
  (:use :cl)
  (:export :+sighup+ :+sigint+ :+sigquit+ :+sigill+ :+sigtrap+
           :+sigabrt+ :+sigemt+ :+sigfpe+ :+sigkill+ :+sigbus+
           :+sigsegv+ :+sigsys+ :+sigpipe+ :+sigalrm+ :+sigterm+
           :+sigurg+ :+sigstop+ :+sigtstp+ :+sigcont+ :+sigchld+
           :+sigttin+ :+sigttou+ :+sigio+ :+sigxcpu+ :+sigxfsz+
           :+sigvtalrm+ :+sigprof+ :+sigwinch+ :+siginfo+ :+sigusr1+ :+sigusr2+

           :signal-name
           :signal-number))

(defpackage trivial-signal
  (:use :cl
        :trivial-signal.signals)
  (:import-from :cffi
                :defcallback
                :foreign-funcall
                :callback)
  (:export :with-signal-handler
           :signal-handler-bind
           :call-signal-handler-bind
           :signal-handler
           :remove-signal-handler
           :remove-all-signal-handlers
           :unix-signal
           :signo
           :signal-name
           :signal-number

           :+sighup+ :+sigint+ :+sigquit+ :+sigill+ :+sigtrap+
           :+sigabrt+ :+sigemt+ :+sigfpe+ :+sigkill+
           :+sigbus+ :+sigsegv+ :+sigsys+ :+sigpipe+ :+sigalrm+
           :+sigterm+ :+sigurg+ :+sigstop+ :+sigtstp+ :+sigcont+
           :+sigchld+ :+sigttin+ :+sigttou+ :+sigio+ :+sigxcpu+
           :+sigxfsz+ :+sigvtalrm+ :+sigprof+ :+sigwinch+ :+siginfo+
           :+sigusr1+ :+sigusr2+))
