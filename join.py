#!/usr/bin/env python3
# Joins the bear raw files into a scheme list
import os

rawpath = "raw"
exclude = ["bear.raw"]

def nsplit(list, n):
    l = []
    for k in range(len(list)/n):
        l.append(list[:n])
        list = list[n:]
    return l

for file in os.listdir(rawpath):
    if file not in exclude:
        print("; " + os.path.splitext(file)[0])
        with open(os.path.join(rawpath, file)) as f:
            for x in f.read().split():
                sign = "2" if "-" in x else "1"
                x = x.replace("-", "")
                sp = x.split(".")
                if len(sp) < 2:
                    sp.append("")
                leftdecim = sp[0].rjust(2, "0")
                rightdecim = sp[1].ljust(6, "0")
                print(leftdecim + rightdecim + sign, end="")
            print()
