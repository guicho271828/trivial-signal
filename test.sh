#!/bin/bash

if which timeout
then
    TIMEOUT=timeout
elif which gtimeout
then
    TIMEOUT=gtimeout
fi

if [[ $LISP == abcl ]]
then
    timelimit=180
else
    timelimit=30
fi
results=

finalize (){
    echo "caught $1 !" >&2
    exit 1
}

for sig in SIGHUP SIGINT SIGTERM SIGXCPU SIGXFSZ
do
    trap "finalize $sig" $sig
done

run-test (){
    for sig in SIGHUP SIGINT SIGTERM SIGXCPU SIGXFSZ
    do
        trap "finalize $sig" $sig
    done
    for target in run{1..6}
    do
        if $TIMEOUT -s 9 $timelimit t/test.ros $target $1
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
