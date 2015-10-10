#!/bin/bash

results=

run-test (){
    for target in run{0..6}
    do
        if t/test.ros $target $1
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
