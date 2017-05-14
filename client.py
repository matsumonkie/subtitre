#!/usr/bin/env python

import socket
import argparse
import json
import time

startPgm = 0.0
start = time.time()
HOST = "localhost"
PORT = 15556
MESSAGE_SIZE = 4096

parser = argparse.ArgumentParser()
parser.add_argument("-s", "--sentence", help="sentence to structure")
args = parser.parse_args()
sentence = args.sentence

socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
socket.connect((HOST, PORT))

#socket.send(unicode(sentence, "utf-8"))
#sentence2 = sentence.encode("utf-8")

socket.send(sentence)
response = socket.recv(MESSAGE_SIZE)
doc = response
print str(doc)

socket.close()

startPgm = (time.time() - start)
print startPgm
print round(startPgm)
