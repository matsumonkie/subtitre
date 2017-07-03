def readJSON file
  JSON.parse(IO.read(Rails.root.join('db', file)))
end

Translation.create(from_lang: "en",
                   to_lang: "fr",
                   site: "wordreference",
                   word: "attack",
                   response: readJSON("wordreference.attack.json"))
