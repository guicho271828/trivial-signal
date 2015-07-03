#!/bin/bash

trap "exit 2" SIGINT

run (){
    which $1 || ( echo "$1 not in path" >&2 ; return 1 )

    for i in {1..8} {10..18} {20..64}
    do
        echo Testing $i
        timeout --kill-after 5 -s $i 5 $@ $i 2>>err | grep Success
    done
}

run sbcl --disable-debugger --load check-mask2.lisp
run ccl --batch --load check-mask2.lisp --
