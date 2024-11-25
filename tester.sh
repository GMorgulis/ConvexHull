#!/bin/bash

# Initialize counters
success_count=0
failure_count=0

run_command() {
    for i in {1..100}; do
        stack run
        status=$?
        if [ $status -eq 0 ]; then
            ((success_count++))
        elif [ $status -eq 1 ]; then
            ((failure_count++))
        else
            echo "Encountered unexpected exit code: $status"
        fi
    done
}

run_command

echo "Success count: $success_count"
echo "Failure count: $failure_count"
