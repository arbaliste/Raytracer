#!/usr/bin/env python3
# Joins the bear raw files into a scheme list
import os

def nsplit(list, n):
    l = []
    for k in range(len(list)/n):
        l.append()
    return l

print("'(", end="")
for file in os.listdir("raw"):

for file in ./raw/bear-*; do
  printf "\n; %s\n" "$(basename $file)"
  printf "(%s)" "$(cat $file | tr -d '\n')"
done
printf ")\n"
