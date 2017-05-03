#!/usr/bin/env python

print "[SERVER] starting"

import socket
import spacy
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

while True:
  socket.listen(MAX_CONNECTIONS)
  client, address = socket.accept()
  print "[SERVER] accepting request"
  request = client.recv(MESSAGE_SIZE)

  doc = nlp(unicode(request, "utf-8"))

  response = "\n".join([(w.text + " " + w.lemma_ + " " + w.pos_) for w in doc])
  client.sendall(response)
  print "[SERVER] replying"
  print ""

print "[SERVER] closing"
client.close()
stock.close()
