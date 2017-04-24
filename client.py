#!/usr/bin/env python

import socket
import argparse
import json

HOST = "localhost"
PORT = 15555
MESSAGE_SIZE = 4096

parser = argparse.ArgumentParser()
parser.add_argument("-s", "--sentence", help="sentence to structure")
args = parser.parse_args()
sentence = args.sentence

socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
socket.connect((HOST, PORT))

socket.send(unicode(sentence, "utf-8"))

response = socket.recv(MESSAGE_SIZE)
doc = response
print str(doc)

socket.close()
