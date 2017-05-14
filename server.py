#!/usr/bin/env python

print "[SERVER] starting"

import socket
import spacy
import time
import signal
import sys

from spacy.tokens import Doc

HOST = 'localhost'
PORT = 15556
MESSAGE_SIZE = 4096
MAX_CONNECTIONS = 1

socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
socket.bind((HOST, PORT))

print "[SERVER] loading spacy"

nlp = spacy.load('en_core_web_sm')

print "[SERVER] listening"
total = 0.0
timePgm = 0.0
socket.listen(MAX_CONNECTIONS)
client = ""

while True:
  try:
  #  print "[SERVER] accepting request"
  #  print "server:before"
  #  print request
  #  print "server:before"
    startPgm = time.time()
    client, address = socket.accept()
    start = time.time()
    request = client.recv(MESSAGE_SIZE)
    doc = nlp(unicode(request, "utf-8"))

    response = "\n".join([(w.text + " " + w.lemma_ + " " + w.pos_) for w in doc])
    client.sendall(response.encode("utf-8"))
    total = total + (time.time() - start)
    timePgm = timePgm + (time.time() - startPgm)
#  print "[SERVER] replying"
#  print ""
  except KeyboardInterrupt:
    if hasattr(client, 'close'):
      print "[SERVER] closing client"
      client.close()
    print "time process"
    print total
    print round(total)
    print "time total"
    print timePgm
    print round(timePgm)
    print "[SERVER] closing"
    sys.exit()
