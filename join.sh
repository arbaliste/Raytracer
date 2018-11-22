#!/bin/bash
# Joins the bear raw files into a scheme list
printf "'("
for file in ./raw/bear-*; do
  printf "\n; %s\n" "$(basename $file)"
  printf "(%s)" "$(cat $file | tr -d '\n')"
done
printf ")\n"
