#!/bin/bash

signals=$(cat <<EOF
HUP
INT
QUIT
ILL
TRAP
ABRT
EMT
FPE
BUS
SEGV
SYS
PIPE
ALRM
TERM
URG
TSTP
CONT
CHLD
TTIN
TTOU
IO
XCPU
XFSZ
VTALRM
PROF
WINCH
INFO
USR1
USR2
EOF
)

# EXIT
# SIGKILL
# SIGSTOP

tmp=$(mktemp tmp.XXXX)

killall(){
    echo "kill -9 $@"
    kill -9 $@ &> /dev/null
}

finalize(){
    killall $pids
    rm -f $tmp
}

pids=

test_signal(){
    echo -n "Test on $signal..."
    sbcl --load signal.lisp ":$signal" &> $tmp &
    pid=$!
    pids="$pid $pids"
    tail -f $tmp --pid $pid | while ps $pid > /dev/null
    do
        read line
        case $line in
            ready)
                echo $line
                ( sleep 10 ; kill -9 $pid &> /dev/null ) &
                echo -n "kill -s SIG$signal $pid"
                if kill -s SIG$signal $pid
                then
                    echo "...signal sent, waiting for sbcl to terminate"
                else
                    echo "...kill(1) failed, force killing sbcl $pid with SIGKILL"
                    kill -9 $pid &> /dev/null
                fi
        esac
    done
    if grep "trivial-signal:" $tmp &> /dev/null
    then
        echo "$signal : Success!"
    fi
}

trap "finalize" EXIT

for signal in $signals
do
    test_signal $signal
done







