#!/bin/bash

maketest (){
    if which $1
    then
        timeout --kill-after 5 25 $@
    else
        echo "$1 not installed"
    fi
}

test1 (){
    maketest $@ binding.lisp
}

for t in test1
do
    echo "running $t"
    $t sbcl --quit --disable-debugger --load
    $t ccl --batch --load
done
