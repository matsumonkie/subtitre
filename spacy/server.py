#!/usr/bin/env python

print "[SERVER] starting"

import spacy
import signal
import sys
from util import *

from spacy.symbols import ORTH, LEMMA, POS
from spacy.tokens import Doc

HOST = 'localhost'
PORT = 15556
MAX_CONNECTIONS = 1

socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
socket.bind((HOST, PORT))

print "[SERVER] loading spacy"

nlp = spacy.load('en_core_web_sm')

nlp.tokenizer.add_special_case(u'<*>', [{ ORTH: u'<*>', LEMMA: u'<*>', POS: u'VERB' }])
nlp.tokenizer.add_special_case(u'<$>', [{ ORTH: u'<$>', LEMMA: u'<$>', POS: u'VERB' }])

print "[SERVER] listening"
socket.listen(MAX_CONNECTIONS)
client = ""

def toToken(word):
  if word.lemma_ == "<*>":
    return " <*> "
  elif word.lemma_ == "<$>":
    return " <$> "
  else:
    return word.text + " " + word.lemma_ + " " + word.pos_

while True:
  try:
    client, address = socket.accept()
    print "[SERVER] accepting request"
    request = recv_msg(client)
    doc = nlp(unicode(request, "utf-8"))
    response = "\n".join([toToken(w) for w in doc])

    print "[SERVER] replying"
    print response
    send_msg(client, response.encode("utf-8"))
  except KeyboardInterrupt:
    if hasattr(client, 'close'):
      print "[SERVER] closing client"
      client.close()
    else:
      print "[SERVER] closing"
    sys.exit()