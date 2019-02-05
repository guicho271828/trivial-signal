#!/bin/bash

trap "exit 2" SIGINT

run (){
    timeout --kill-after 5 -s $2 5 ros -L $1 test.ros $2 2> $1-$2.err
    [ -s $1-$2.err ] || rm $1-$2.err
}
export -f run

SHELL=/bin/bash

parallel run ::: sbcl-bin ccl-bin ::: {1..8} {10..18} {20..64}
