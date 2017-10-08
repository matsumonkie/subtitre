print("loading...")

import spacy
import signal
import sys
from util import *
import time
import asyncio
import redis

from spacy.symbols import ORTH, LEMMA, POS
from spacy.tokens import Doc

HOST = 'localhost'
PORT = 6379
DB = 0

nlp = { 'en': spacy.load('en'),
        'fr': spacy.load('fr') }

for key in nlp:
  nlp[key].tokenizer.add_special_case(u'<*>', [{ ORTH: u'<*>', LEMMA: u'<*>', POS: u'VERB' }])
  nlp[key].tokenizer.add_special_case(u'<$>', [{ ORTH: u'<$>', LEMMA: u'<$>', POS: u'VERB' }])

print("loaded")

def toToken(word):
  if word.lemma_ == "<*>":
    return " <*> "
  elif word.lemma_ == "<$>":
    return " <$> "
  else:
    return word.text + " " + word.lemma_ + " " + word.pos_

def handler(message):
  _, lang, id = message['channel'].decode("utf-8").split(":")
  content = message['data']
  doc = nlp[lang](content.decode("utf-8"))
  response = "\n".join([toToken(w) for w in doc])
  channel = "spacified:{}:{}".format(lang, id)
  client.publish(channel, response)

client = redis.StrictRedis(host=HOST, port=PORT, db=DB)
pubsub = client.pubsub()
loop = asyncio.get_event_loop()
try:
  pubsub.psubscribe(**{ 'spacify:en:*': handler,
                        'spacify:fr:*': handler })
  thread = pubsub.run_in_thread(sleep_time=0.001)
  loop.run_forever()
except KeyboardInterrupt:
  pass
finally:
  pubsub.close()
  loop.close()
