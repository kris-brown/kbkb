#!/bin/bash
mkdir "$2 $1"
printf "%% TAG Prop\ntest\n" > "$2 $1/$1.tex"
echo "test" > "$2 $1/Proof.tex"