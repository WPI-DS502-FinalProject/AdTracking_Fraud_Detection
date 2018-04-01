#!/usr/bin/env python

ftrain = open('train.csv')
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
