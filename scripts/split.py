#!/usr/bin/env python

ftrainno = open('train_no.csv')

line = ftrainno.readline()
fsplit = open('train_%s.csv'%(0), 'w+')

c = 0
i = 1
while line:
  fsplit.write(line)
  if c == 500000:
    fsplit.close() 
    fsplit = open('train_%s.csv'%(i), 'w+')
    c = 0
    i += 1
   
  line = ftrainno.readline()
  c += 1
