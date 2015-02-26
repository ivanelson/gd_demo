#-*- encoding: utf-8 -*-


for i in range(100):
    if i % 2 == 0: print "eh par: %s" % i

import this

import subprocess

subprocess.call(['echo','oi',shell=True])
