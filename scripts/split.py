#!/usr/bin/env python
import sys

ftrainno = open(sys.argv[1])

line = ftrainno.readline()
fsplit = open('zeros_%s.csv'%(0), 'w+')

c = 0
i = 1
while line:
  fsplit.write(line)
  if c == 500000:
    fsplit.close() 
    fsplit = open('zeros_%s.csv'%(i), 'w+')
    c = 0
    i += 1
   
  line = ftrainno.readline()
  c += 1
