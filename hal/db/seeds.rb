def readJSON file
  JSON.parse(IO.read(Rails.root.join('db', file)))
end

Wordreference.create(from: "en",
                     to: "fr",
                     word: "attack",
                     response: readJSON("wordreference.attack.json"))
