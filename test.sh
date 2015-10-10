#!/bin/bash

if which timeout
then
    TIMEOUT=timeout
elif which gtimeout
then
    TIMEOUT=gtimeout
fi

results=

finalize (){
    echo "caught $1 !" >&2
    exit 1
}

for sig in SIGHUP SIGINT SIGQUIT SIGILL SIGTRAP SIGABRT SIGBUS SIGFPE SIGKILL SIGUSR1 SIGSEGV SIGUSR2 SIGPIPE SIGALRM SIGTERM SIGSTKFLT SIGCHLD SIGCONT SIGSTOP SIGTSTP SIGTTIN SIGTTOU SIGURG SIGXCPU SIGXFSZ SIGVTALRM SIGPROF SIGWINCH SIGIO SIGPWR SIGSYS SIGRTMIN SIGRTMIN+1 SIGRTMIN+2 SIGRTMIN+3 SIGRTMIN+4 SIGRTMIN+5 SIGRTMIN+6 SIGRTMIN+7 SIGRTMIN+8 SIGRTMIN+9 SIGRTMIN+10 SIGRTMIN+11 SIGRTMIN+12 SIGRTMIN+13 SIGRTMIN+14 SIGRTMIN+15 SIGRTMAX-14 SIGRTMAX-13 SIGRTMAX-12 SIGRTMAX-11 SIGRTMAX-10 SIGRTMAX-9 SIGRTMAX-8 SIGRTMAX-7 SIGRTMAX-6 SIGRTMAX-5 SIGRTMAX-4 SIGRTMAX-3 SIGRTMAX-2 SIGRTMAX-1 SIGRTMAX
do
    trap "finalize $sig" $sig
done

run-test (){
    for sig in SIGHUP SIGINT SIGQUIT SIGILL SIGTRAP SIGABRT SIGBUS SIGFPE SIGKILL SIGUSR1 SIGSEGV SIGUSR2 SIGPIPE SIGALRM SIGTERM SIGSTKFLT SIGCHLD SIGCONT SIGSTOP SIGTSTP SIGTTIN SIGTTOU SIGURG SIGXCPU SIGXFSZ SIGVTALRM SIGPROF SIGWINCH SIGIO SIGPWR SIGSYS SIGRTMIN SIGRTMIN+1 SIGRTMIN+2 SIGRTMIN+3 SIGRTMIN+4 SIGRTMIN+5 SIGRTMIN+6 SIGRTMIN+7 SIGRTMIN+8 SIGRTMIN+9 SIGRTMIN+10 SIGRTMIN+11 SIGRTMIN+12 SIGRTMIN+13 SIGRTMIN+14 SIGRTMIN+15 SIGRTMAX-14 SIGRTMAX-13 SIGRTMAX-12 SIGRTMAX-11 SIGRTMAX-10 SIGRTMAX-9 SIGRTMAX-8 SIGRTMAX-7 SIGRTMAX-6 SIGRTMAX-5 SIGRTMAX-4 SIGRTMAX-3 SIGRTMAX-2 SIGRTMAX-1 SIGRTMAX
    do
        trap "finalize $sig" $sig
    done
    for target in run{1..6}
    do
        if $TIMEOUT -s 9 10 t/test.ros $target $1
        then
            echo "✔ t/test.ros $target $1"
        else
            echo "✘ t/test.ros $target $1"
            return 1
        fi
    done
    echo "✔ all tests for $1 passed"
    results="$1 $results"
}

for signo in {1..64}
do
    run-test $signo
done



if [ -z $results ]
then
    echo "No signals available!"
    exit 1
else
    echo "Available signals: $results"
    exit 0
fi
