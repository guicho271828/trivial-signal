#!/bin/bash

maketest (){
    if which $1
    then
        timeout --kill-after 5 25 $@ 2>&1
        local status=$?
        echo "IMPL:$1 SIGNAL: $i EXIT_STATUS: $status NAME: SIG$(kill -l $i)" >&2
        return $status
    else
        echo "$1 not installed" >&3
        return 1
    fi
}

sub (){
    i=$1 ; shift
    if maketest $@ check-mask.lisp -- $i > $1.$i.log
    then
        touch $1.$i.success
    else
        touch $1.$i.fail
    fi
}

aggregate (){
    for f in *.$1
    do
        echo $(basename $f .$1) >> $2.$1_unsrt
    done
    sort $2.$1_unsrt > $2.$1
}

test2 (){
    rm $1.*
    for i in {0..31}
    do
        sub $i $@
    done
    wait
    aggregate success $1
    aggregate fail $1
}

trap "exit 2" SIGINT

run (){
    for t in test2
    do
        echo "running $t"
        $t sbcl --quit --disable-debugger --load
        $t ccl --batch --load
    done
}

run

