# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rails db:seed command (or created alongside the database with db:setup).
#
# Examples:
#
#   movies = Movie.create([{ name: 'Star Wars' }, { name: 'Lord of the Rings' }])
#   Character.create(name: 'Luke', movie: movies.first)

Wordreference.create({ from: "en",
                       to: "fr",
                       word: "attack",
                       response: { term0: {
                                     PrincipalTranslations: {
	                               "0" => {
		                         OriginalTerm: { term: "attack", POS: "n", sense: "assault", usage: ""},
		                         FirstTranslation: { term: "attaque, agression", POS: "nf", sense: ""}, Note: ""},
	                               "1" => {
		                         OriginalTerm: { term: "attack", POS: "n", sense: "military: offensive operation", usage: ""},
		                               FirstTranslation: { term: "attaque", POS: "nf", sense: "Militaire"},
		                               SecondTranslation: { term: "assaut", POS: "nm", sense: ""}, Note: ""},
                                       AdditionalTranslations: {
	                                 "0" => {
		                           OriginalTerm: { term: "attack", POS: "vi", sense: "act with hostility", usage: ""},
		                           FirstTranslation: { term: "agresser", POS: "vtr", sense: ""}, Note: ""},
	                                 "1" => {
		                           OriginalTerm: { term: "attack, replace: attack sth", POS: "vtr", sense: "set about vigorously", usage: "figurative"},
		                           FirstTranslation: { term: "s'attaquer à", POS: "v pron + prép", sense: "problème"}, Note: ""}
                                       }
                                     },
                                     Lines: "End Reached",
                                     END: true
                                   }
                                 }})
