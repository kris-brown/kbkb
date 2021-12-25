#!/bin/bash
mkdir "$2 Exercise $1"
printf "%% TAG Exercise\ntest\n" > "$2 Exercise $1/Exercise $1.tex"
echo "test" > "$2 Exercise $1/Solution.tex"