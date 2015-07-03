(in-package :trivial-signal.signals)

(include "signal.h")

;; posix.1-1990

(constant (+SIGHUP+ "SIGHUP") :documentation "Terminal line hangup. ")
(constant (+SIGINT+ "SIGINT") :documentation "Interrupt program. ")
(constant (+SIGQUIT+ "SIGQUIT") :documentation "Quit program. ")
(constant (+SIGILL+ "SIGILL") :documentation "Illegal instruction. ")
(constant (+SIGABRT+ "SIGABRT") :documentation "Abort program (formerly SIGIOT). ")
(constant (+SIGFPE+ "SIGFPE") :documentation "Floating-point exception. ")
(constant (+SIGKILL+ "SIGKILL") :documentation "Kill program. Cannot be caught, blocked, or ignored. ")
(constant (+SIGSEGV+ "SIGSEGV") :documentation "Segmentation violation. ")
(constant (+SIGPIPE+ "SIGPIPE") :documentation "Write on a pipe with no reader. ")
(constant (+SIGALRM+ "SIGALRM") :documentation "Real-time timer expired. ")
(constant (+SIGTERM+ "SIGTERM") :documentation "Software termination signal. ")
(constant (+SIGUSR1+ "SIGUSR1") :documentation "User defined signal 1. ")
(constant (+SIGUSR2+ "SIGUSR2") :documentation "User defined signal 2. ")
(constant (+SIGCONT+ "SIGCONT") :documentation "Continue after stop. ")
(constant (+SIGCHLD+ "SIGCHLD") :documentation "Child status has changed. ")
(constant (+SIGSTOP+ "SIGSTOP") :documentation "Stop. Cannot be caught, blocked, or ignored. ")
(constant (+SIGTSTP+ "SIGTSTP") :documentation "Stop signal generated from keyboard. ")
(constant (+SIGTTIN+ "SIGTTIN") :documentation "Background read attempted from control terminal. ")
(constant (+SIGTTOU+ "SIGTTOU") :documentation "Background write attempted to control terminal. ")

;; posix.1-2001, SUSv2
(constant (+SIGBUS+ "SIGBUS") :documentation "Bus error. ")
(constant (+SIGPOLL+ "SIGPOLL") :documentation "Pollable event (Sys V). Synonym of SIGIO. ")
(constant (+SIGPROF+ "SIGPROF") :documentation "Profiling timer alarm (see setitimer(2)). ")
(constant (+SIGSYS+ "SIGSYS") :documentation "Non-existent system call invoked. ")
(constant (+SIGTRAP+ "SIGTRAP") :documentation "Trace trap. ")
(constant (+SIGURG+ "SIGURG") :documentation "Urgent condition present on socket. ")
(constant (+SIGVTALRM+ "SIGVTALRM") :documentation "Virtual time alarm (see setitimer(2)). ")
(constant (+SIGXCPU+ "SIGXCPU") :documentation "Cpu time limit exceeded (see setrlimit(2)). ")
(constant (+SIGXFSZ+ "SIGXFSZ") :documentation "File size limit exceeded (see setrlimit(2)). ")

;; not in posix

(constant (+SIGIOT+ "SIGIOT") :documentation "IOT trap. A synonym for SIGABRT. ")
(constant (+SIGEMT+ "SIGEMT") :documentation "Emulate instruction executed. ")

(constant (+SIGTKFLT+ "SIGTKFLT") :documentation "Stack fault on coprocessor (unused on linux)")
(constant (+SIGIO+ "SIGIO") :documentation "I/O is possible on a descriptor (see fcntl(2)). ")
(constant (+SIGCLD+ "SIGCLD") :documentation "A synonym for SIGCHLD.")
(constant (+SIGPWR+ "SIGPWR") :documentation "Power failure (System V)")
(constant (+SIGINFO+ "SIGINFO") :documentation
          #+bsd "Status request from keyboard."
          #+linux "Synonym for SIGPWR")
(constant (+SIGLOST+ "SIGLOST") :documentation "File lock lost")
(constant (+SIGWINCH+ "SIGWINCH") :documentation "Window size change. ")
(constant (+SIGUNUSED+ "SIGUNUSED") :documentation "Unused signal (will be SIGSYS)")

;; realtime signals (POSIX.1-2001)

(constant (+SIGRTMIN+ "SIGRTMIN") :documentation "SIGRTMIN in POSIX.1-2001, real time signal")
(constant (+SIGRTMIN+1+ "SIGRTMIN+1") :documentation "SIGRTMIN+1 in POSIX.1-2001, real time signal")
(constant (+SIGRTMIN+2+ "SIGRTMIN+2") :documentation "SIGRTMIN+2 in POSIX.1-2001, real time signal")
(constant (+SIGRTMIN+3+ "SIGRTMIN+3") :documentation "SIGRTMIN+3 in POSIX.1-2001, real time signal")
(constant (+SIGRTMIN+4+ "SIGRTMIN+4") :documentation "SIGRTMIN+4 in POSIX.1-2001, real time signal")
(constant (+SIGRTMIN+5+ "SIGRTMIN+5") :documentation "SIGRTMIN+5 in POSIX.1-2001, real time signal")
(constant (+SIGRTMIN+6+ "SIGRTMIN+6") :documentation "SIGRTMIN+6 in POSIX.1-2001, real time signal")
(constant (+SIGRTMIN+7+ "SIGRTMIN+7") :documentation "SIGRTMIN+7 in POSIX.1-2001, real time signal")
(constant (+SIGRTMIN+8+ "SIGRTMIN+8") :documentation "SIGRTMIN+8 in POSIX.1-2001, real time signal")
(constant (+SIGRTMIN+9+ "SIGRTMIN+9") :documentation "SIGRTMIN+9 in POSIX.1-2001, real time signal")
(constant (+SIGRTMIN+10+ "SIGRTMIN+10") :documentation "SIGRTMIN+10 in POSIX.1-2001, real time signal")
(constant (+SIGRTMIN+11+ "SIGRTMIN+11") :documentation "SIGRTMIN+11 in POSIX.1-2001, real time signal")
(constant (+SIGRTMIN+12+ "SIGRTMIN+12") :documentation "SIGRTMIN+12 in POSIX.1-2001, real time signal")
(constant (+SIGRTMIN+13+ "SIGRTMIN+13") :documentation "SIGRTMIN+13 in POSIX.1-2001, real time signal")
(constant (+SIGRTMIN+14+ "SIGRTMIN+14") :documentation "SIGRTMIN+14 in POSIX.1-2001, real time signal")
(constant (+SIGRTMIN+15+ "SIGRTMIN+15") :documentation "SIGRTMIN+15 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX-14+ "SIGRTMAX-14") :documentation "SIGRTMAX-14 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX-13+ "SIGRTMAX-13") :documentation "SIGRTMAX-13 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX-12+ "SIGRTMAX-12") :documentation "SIGRTMAX-12 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX-11+ "SIGRTMAX-11") :documentation "SIGRTMAX-11 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX-10+ "SIGRTMAX-10") :documentation "SIGRTMAX-10 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX-9+ "SIGRTMAX-9") :documentation "SIGRTMAX-9 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX-8+ "SIGRTMAX-8") :documentation "SIGRTMAX-8 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX-7+ "SIGRTMAX-7") :documentation "SIGRTMAX-7 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX-6+ "SIGRTMAX-6") :documentation "SIGRTMAX-6 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX-5+ "SIGRTMAX-5") :documentation "SIGRTMAX-5 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX-4+ "SIGRTMAX-4") :documentation "SIGRTMAX-4 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX-3+ "SIGRTMAX-3") :documentation "SIGRTMAX-3 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX-2+ "SIGRTMAX-2") :documentation "SIGRTMAX-2 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX-1+ "SIGRTMAX-1") :documentation "SIGRTMAX-1 in POSIX.1-2001, real time signal")
(constant (+SIGRTMAX+ "SIGRTMAX") :documentation "SIGRTMAX in POSIX.1-2001, real time signal")

;; bsd signals https://www.freebsd.org/cgi/man.cgi?sektion=3&query=signal

(constant (+SIGTHR+ "SIGTHR") :documentation "Thread interrupt. ")
(constant (+SIGLIBRT+ "SIGLIBRT") :documentation "Real-time library interrupt. ")




