#!/usr/bin/env python
import sys

ftrain = open(sys.argv[1])
ftrain.readline()
line = ftrain.readline()

fyes = open('train_yes.csv', 'w+')
fno = open('train_no.csv', 'w+')

while line:
  is_attr = int(line.strip().split(',')[-1], 10)
  if is_attr == 1:
    fyes.write(line)
  else:
    fno.write(line)
  line = ftrain.readline()
