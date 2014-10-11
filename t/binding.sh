#!/bin/bash

run_test (){
    if which $1
    then
        $@ binding.lisp &
        pid=$!
        sleep 15
        echo "killing $pid with -9"
        kill -9 $pid
    else
        echo "$1 not installed"
    fi
}

run_test sbcl --quit --load
run_test ccl --batch --load

