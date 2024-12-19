#!/bin/bash

echo "here"

file=$1

run_command() {
  local n=$1
  stack run -- +RTS -N$n -s -ls -RTS $file
}

for n in {1..8}; do
    echo "Iteration $iteration, running with -N$n"
    run_command $n
done


