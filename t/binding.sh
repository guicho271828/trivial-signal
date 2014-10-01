#!/bin/bash


sbcl --load binding.lisp &
pid=$!
sleep 30
echo "killing $pid with -9"
kill -9 $pid


