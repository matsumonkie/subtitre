import argparse
import spacy

parser = argparse.ArgumentParser()
parser.add_argument("-s", "--sentence", help="sentence to structure")
args = parser.parse_args()
sentence = args.sentence

nlp = spacy.load('en_core_web_sm')

doc = nlp(unicode(sentence, "utf-8"))

for w in doc:
  print w.lemma_, w.tag_
