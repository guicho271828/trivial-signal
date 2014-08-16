(in-package :cl-user)
(defpackage trivial-signal.signals
  (:use :cl)
  (:export :+sighup+
           :+sigint+
           :+sigquit+
           :+sigill+
           :+sigtrap+
           :+sigabrt+
           :+sigemt+
           :+sigfpe+
           :+sigkill+
           :+sigbus+
           :+sigsegv+
           :+sigsys+
           :+sigpipe+
           :+sigalrm+
           :+sigterm+
           :+sigurg+
           :+sigstop+
           :+sigtstp+
           :+sigcont+
           :+sigchld+
           :+sigttin+
           :+sigttou+
           :+sigio+
           :+sigxcpu+
           :+sigxfsz+
           :+sigvtalrm+
           :+sigprof+
           :+sigwinch+
           :+siginfo+
           :+sigusr1+
           :+sigusr2+

           :signal-name
           :signal-number))
(in-package :trivial-signal.signals)

#.`(progn
     ,@(loop for (name num docstring) in '((HUP     1 "terminal line hangup.")
                                           (INT     2 "interrupt program.")
                                           (QUIT    3 "quit program.")
                                           (ILL     4 "illegal instruction.")
                                           (TRAP    5 "trace trap.")
                                           (ABRT    6 "abort program (formerly SIGIOT).")
                                           (EMT     7 "emulate instruction executed.")
                                           (FPE     8 "floating-point exception.")
                                           (KILL    9 "kill program. (cannot be caught or ignored)")
                                           (BUS    10 "bus error.")
                                           (SEGV   11 "segmentation violation.")
                                           (SYS    12 "non-existent system call invoked.")
                                           (PIPE   13 "write on a pipe with no reader.")
                                           (ALRM   14 "real-timeimer expired.")
                                           (TERM   15 "software termination signal.")
                                           (URG    16 "urgent condition present on socket.")
                                           (STOP   17 "stop (cannot be caught or ignored).")
                                           (TSTP   18 "stop signal generated from keyboard.")
                                           (CONT   19 "continue after stop.")
                                           (CHLD   20 "child status has changed.")
                                           (TTIN   21 "background read attempted from control terminal.")
                                           (TTOU   22 "background write attempted from control terminal.")
                                           (IO     23 "I/O is possible on a descriptor (see fcntl(2)).")
                                           (XCPU   24 "cpuime limit exceeded (see setrlimit(2)).")
                                           (XFSZ   25 "file size limit exceeded (see setrlimit(2)).")
                                           (VTALRM 26 "virtualime alarm (see setitimer(2)).")
                                           (PROF   27 "profilingimer alarm (see setitimer(2)).")
                                           (WINCH  28 "Window size change.")
                                           (INFO   29 "status request from keyboard.")
                                           (USR1   30 "User defined signal 1.")
                                           (USR2   31 "User defined signal 2."))
          ;; FIXME: these signals are OS-dependent, and sometimes a single
          ;; signal has multiple signo.  For example, on Linux USR1 is
          ;; assigned to 30,10 and 16.  Or, at least it is still possible
          ;; to specify which the version of POSIX spec it is based on.

          ;; Alternatively, the more reliable way to take the valid signo
          ;; for each OS is using cffi-grovel, which reads the header file
          ;; in that system and load the correct value.
             collect `(defconstant ,(intern (format nil "+~A~A+" (string :sig) name)) ,num ,docstring) into defconstants
             collect `(,num ,(intern (symbol-name name) :keyword)) into signal-name-rules
             collect `(,(intern (symbol-name name) :keyword) ,num) into signal-num-rules
             finally
                (return
                  `((defun signal-name (signo)
                      "Return the name of SIGNO as a keyword."
                      (ecase signo
                        ,@signal-name-rules))
                    (defun signal-number (signame)
                      "Return the number of SIGNAME as an integer."
                      (ecase signame
                        ,@signal-num-rules))
                    ,@defconstants))))
