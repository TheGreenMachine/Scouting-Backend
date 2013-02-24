#!/usr/bin/python3
import os
import fileinput

for filename in os.listdir():
    if filename == 'rmnewline.py':
        continue
    with fileinput.input(files=[filename], inplace=True) as handle:
        for line in handle:
            if line.strip():
                print(line.strip())
