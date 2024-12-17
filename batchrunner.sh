#!/bin/bash

echo "here"

# Function to run the command with specified number of threads
run_command() {
  local n=$1
  stack run -- +RTS -N$n -s -ls -RTS r4m.txt
}

# Loop to run the command 10 times in sequence from 1 to 8 threads
for n in {1..8}; do
    echo "Iteration $iteration, running with -N$n"
    run_command $n
done


echo "All runs completed."
