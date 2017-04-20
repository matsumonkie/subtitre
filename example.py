import spacy
nlp = spacy.load('en_core_web_sm')

doc = nlp(u'I want to meet you !')

for w in doc:
  print w.lemma_, w.tag_


print "end"

# Doc [Token]
