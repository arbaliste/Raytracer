#!/usr/bin/env python3
# Joins the bear raw files into a scheme list
import os

rawpath = "raw"
exclude = ["bear.raw"]

def nsplit(list, n):
    l = []
    for _ in range(len(list)//n):
        l.append(list[:n])
        list = list[n:]
    return l

def process(num, invert=False):
    sign = "2" if ("-" in num) != invert else "1"
    num = num.replace("-", "")
    sp = num.split(".")
    if len(sp) < 2:
        sp.append("")
    leftdecim = sp[0].rjust(2, "0")
    rightdecim = sp[1].ljust(6, "0")
    return leftdecim + rightdecim + sign

for file in os.listdir(rawpath):
    if file not in exclude:
        print("; " + os.path.splitext(file)[0])
        with open(os.path.join(rawpath, file)) as f:
            for x, z, y in nsplit(f.read().split(), 3):
                print(process(x) + process(y) + process(z, True), end="")
            print()
