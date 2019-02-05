(in-package :trivial-signal.signals)

(include "signal.h")

;; posix.1-1990

(constant (+SIGHUP+ "SIGHUP")   :optional t :documentation "Terminal line hangup. ")
(constant (+SIGINT+ "SIGINT")   :optional t :documentation "Interrupt program. ")
(constant (+SIGQUIT+ "SIGQUIT") :optional t :documentation "Quit program. ")
(constant (+SIGILL+ "SIGILL")   :optional t :documentation "Illegal instruction. ")
(constant (+SIGABRT+ "SIGABRT") :optional t :documentation "Abort program (formerly SIGIOT). ")
(constant (+SIGFPE+ "SIGFPE")   :optional t :documentation "Floating-point exception. ")
(constant (+SIGKILL+ "SIGKILL") :optional t :documentation "Kill program. Cannot be caught, blocked, or ignored. ")
(constant (+SIGSEGV+ "SIGSEGV") :optional t :documentation "Segmentation violation. ")
(constant (+SIGPIPE+ "SIGPIPE") :optional t :documentation "Write on a pipe with no reader. ")
(constant (+SIGALRM+ "SIGALRM") :optional t :documentation "Real-time timer expired. ")
(constant (+SIGTERM+ "SIGTERM") :optional t :documentation "Software termination signal. ")
(constant (+SIGUSR1+ "SIGUSR1") :optional t :documentation "User defined signal 1. ")
(constant (+SIGUSR2+ "SIGUSR2") :optional t :documentation "User defined signal 2. ")
(constant (+SIGCONT+ "SIGCONT") :optional t :documentation "Continue after stop. ")
(constant (+SIGCHLD+ "SIGCHLD") :optional t :documentation "Child status has changed. ")
(constant (+SIGSTOP+ "SIGSTOP") :optional t :documentation "Stop. Cannot be caught, blocked, or ignored. ")
(constant (+SIGTSTP+ "SIGTSTP") :optional t :documentation "Stop signal generated from keyboard. ")
(constant (+SIGTTIN+ "SIGTTIN") :optional t :documentation "Background read attempted from control terminal. ")
(constant (+SIGTTOU+ "SIGTTOU") :optional t :documentation "Background write attempted to control terminal. ")

;; posix.1-2001, SUSv2
(constant (+SIGBUS+ "SIGBUS")       :optional t :documentation "Bus error. ")
(constant (+SIGPOLL+ "SIGPOLL")     :optional t :documentation "Pollable event (Sys V). Synonym of SIGIO. ")
(constant (+SIGPROF+ "SIGPROF")     :optional t :documentation "Profiling timer alarm (see setitimer(2)). ")
(constant (+SIGSYS+ "SIGSYS")       :optional t :documentation "Non-existent system call invoked. ")
(constant (+SIGTRAP+ "SIGTRAP")     :optional t :documentation "Trace trap. ")
(constant (+SIGURG+ "SIGURG")       :optional t :documentation "Urgent condition present on socket. ")
(constant (+SIGVTALRM+ "SIGVTALRM") :optional t :documentation "Virtual time alarm (see setitimer(2)). ")
(constant (+SIGXCPU+ "SIGXCPU")     :optional t :documentation "Cpu time limit exceeded (see setrlimit(2)). ")
(constant (+SIGXFSZ+ "SIGXFSZ")     :optional t :documentation "File size limit exceeded (see setrlimit(2)). ")

;; not in posix

(constant (+SIGIOT+ "SIGIOT")       :optional t :documentation "IOT trap. A synonym for SIGABRT. ")
(constant (+SIGEMT+ "SIGEMT")       :optional t :documentation "Emulate instruction executed. ")
(constant (+SIGTKFLT+ "SIGTKFLT")   :optional t :documentation "Stack fault on coprocessor (unused on linux)")
(constant (+SIGIO+ "SIGIO")         :optional t :documentation "I/O is possible on a descriptor (see fcntl(2)). ")
(constant (+SIGCLD+ "SIGCLD")       :optional t :documentation "A synonym for SIGCHLD.")
(constant (+SIGPWR+ "SIGPWR")       :optional t :documentation "Power failure (System V)")
(constant (+SIGINFO+ "SIGINFO")     :optional t :documentation "(BSD) Status request from keyboard. (System V) Synonym for SIGPWR.")
(constant (+SIGLOST+ "SIGLOST")     :optional t :documentation "File lock lost")
(constant (+SIGWINCH+ "SIGWINCH")   :optional t :documentation "Window size change. ")
(constant (+SIGUNUSED+ "SIGUNUSED") :optional t :documentation "Unused signal (will be SIGSYS)")

;; realtime signals (POSIX.1-2001)

(constant (+SIGRTMIN+ "SIGRTMIN")       :optional t :documentation "SIGRTMIN in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMIN+1+ "SIGRTMIN+1")   :optional t :documentation "SIGRTMIN+1 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMIN+2+ "SIGRTMIN+2")   :optional t :documentation "SIGRTMIN+2 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMIN+3+ "SIGRTMIN+3")   :optional t :documentation "SIGRTMIN+3 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMIN+4+ "SIGRTMIN+4")   :optional t :documentation "SIGRTMIN+4 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMIN+5+ "SIGRTMIN+5")   :optional t :documentation "SIGRTMIN+5 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMIN+6+ "SIGRTMIN+6")   :optional t :documentation "SIGRTMIN+6 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMIN+7+ "SIGRTMIN+7")   :optional t :documentation "SIGRTMIN+7 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMIN+8+ "SIGRTMIN+8")   :optional t :documentation "SIGRTMIN+8 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMIN+9+ "SIGRTMIN+9")   :optional t :documentation "SIGRTMIN+9 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMIN+10+ "SIGRTMIN+10") :optional t :documentation "SIGRTMIN+10 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMIN+11+ "SIGRTMIN+11") :optional t :documentation "SIGRTMIN+11 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMIN+12+ "SIGRTMIN+12") :optional t :documentation "SIGRTMIN+12 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMIN+13+ "SIGRTMIN+13") :optional t :documentation "SIGRTMIN+13 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMIN+14+ "SIGRTMIN+14") :optional t :documentation "SIGRTMIN+14 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMIN+15+ "SIGRTMIN+15") :optional t :documentation "SIGRTMIN+15 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMAX-14+ "SIGRTMAX-14") :optional t :documentation "SIGRTMAX-14 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMAX-13+ "SIGRTMAX-13") :optional t :documentation "SIGRTMAX-13 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMAX-12+ "SIGRTMAX-12") :optional t :documentation "SIGRTMAX-12 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMAX-11+ "SIGRTMAX-11") :optional t :documentation "SIGRTMAX-11 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMAX-10+ "SIGRTMAX-10") :optional t :documentation "SIGRTMAX-10 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMAX-9+ "SIGRTMAX-9")   :optional t :documentation "SIGRTMAX-9 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMAX-8+ "SIGRTMAX-8")   :optional t :documentation "SIGRTMAX-8 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMAX-7+ "SIGRTMAX-7")   :optional t :documentation "SIGRTMAX-7 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMAX-6+ "SIGRTMAX-6")   :optional t :documentation "SIGRTMAX-6 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMAX-5+ "SIGRTMAX-5")   :optional t :documentation "SIGRTMAX-5 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMAX-4+ "SIGRTMAX-4")   :optional t :documentation "SIGRTMAX-4 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMAX-3+ "SIGRTMAX-3")   :optional t :documentation "SIGRTMAX-3 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMAX-2+ "SIGRTMAX-2")   :optional t :documentation "SIGRTMAX-2 in POSIX.1-2001, real time signal")
;; (constant (+SIGRTMAX-1+ "SIGRTMAX-1")   :optional t :documentation "SIGRTMAX-1 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX+ "SIGRTMAX")       :optional t :documentation "SIGRTMAX in POSIX.1-2001, real time signal")

;; bsd signals https://www.freebsd.org/cgi/man.cgi?sektion=3&query=signal

(constant (+SIGTHR+ "SIGTHR")     :optional t :documentation "Thread interrupt. ")
(constant (+SIGLIBRT+ "SIGLIBRT") :optional t :documentation "Real-time library interrupt. ")




