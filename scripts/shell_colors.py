#!/usr/bin/env python3

fg = '\033[38;5;'
bg = '\033[48;5;'

for i in range( 0, 256):
    if i % 16 == 0:
        print()
    n = str(i)
    m = str(i).ljust(3)
    fgstr = fg + n + 'm' + m
    bgstr = bg + n + 'm' 'XXXXX'
    print(fgstr, bgstr, '\033[0m', end=' ')
print()
