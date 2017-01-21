(def bob-too-bad [
  "Sadder than a map!"
  "A blamed pity."
  "Oh, {p:he's} in a bad loaf now."
  "Well, that's how the cookie crumbles."
  "Better {p:him} than me."
  "Sic semper contestantus."
  "Epic fail!"
  "TS."
  "Tough darts."
  "Ain't that a bite!"
  "Miz!"
  "Nebekh!"
  "Très bummer."
  "Sad trombone."
  "Oh, I'm sorry!"
  "The agony of defeat!"
  "Shazbot!"
  "Press F to pay respects."])

(def gadget-adjectives [
  "crazy"
  "cryptic"
  "mysterious"
  "enigmatic"
  "Art Deco"
  "clockwork"
  "futuristic"
  "alien"
  "modern"
  "shiny"
  "rusty"
  "antique"
  "vintage"
  "ivory"
  "wooden"
  "brass"
  "silvery"
  "stainless-steel"
  "matte"
  "flimsy"
  "rugged"
  "plastic"
  "Bakelite"
  "nylon"
  "PVC"
  "tiny"
  "heavy"
  "boxy"
  "sleek"
  "bulky"
  "crude"])

(def soda-cans [
  "blank"
  "foreign-language-labeled"
  "polka-dot"
  "striped"
  "garishly colored"
  "off-white"
  "red-checked"
  "houndstooth"
  "paisley"
  "reddish-greenish"
  "tie-dye"
  "reflective"
  ; The idea for the color words is to use ones that appear in
  ; none of Rogue 5.4.4, NetHack, or Angband. (Some may be possible
  ; hallucinated colors in NetHack.) Also, the weirder the better.
  "cornflower-blue"
  "periwinkle"
  "verdigris"
  "teal"
  "lovat"
  "luteous"
  "maroon"
  "rufous"
  "scarlet"
  "hot-pink"
  "mauve"
  "gamboge"
  "fulvous"
  "goldenrod"
  "nankeen"])

(def gift-box-labels [
  "1.21GW"              ; Back to the Future
  "1729"                ; The Hardy–Ramanujan number
  "18156248"            ; PMID of "Sex, aggression, and humour: responses to unicycling"
  "2038019"             ; 2038-01-19 is the Unix doomsday
  "23.36664"            ; sqrt(546), from MLP's Failure Song
  "25212683"            ; PMID of my first published article
  "2FA3E1B"             ; Initial commit of Rogue TV
  "32W353Y"             ; 0xDEADBEEF in RFC 4648 Base32
  "41421356"            ; sqrt(2)
  "6ACCDAE13EFF7I3L"    ; First half of Newton's anagram
  "794.8"               ; Dewey Decimal for computer games
  "9N4O4QRR4S8T12UX"    ; Second half of Newton's anagram
  "AA23C2187"           ; Princess Leia was in detention block AA-23, cell 2187
  "ARGELFRASTER"        ; Enchanted Forest Chronicles
  "BIDOOF"              ; A Pokémon
  "BLINKENLICHTEN"      ; http://www.catb.org/jargon/html/B/blinkenlights.html
  "CHIM"                ; http://www.uesp.net/wiki/Lore:CHIM
  "COSMICOSMO"          ; Cosmic Osmo
  "DOOTDOOT"            ; http://knowyourmeme.com/memes/skull-trumpet
  "E102G"               ; E-102 Gamma (Sonic Adventure)
  "ETNOMAILGAT"         ; Paul Tagliamonte's (creator of Hy) last name backwards
  "FRINDLE"             ; The book of the same name
  "GIPEMOS"             ; "Some pig" (Charlotte's Web) backwards
  "GOOZACK"             ; Wayside School
  "IDDQD"               ; Doom cheat code
  "KBA"                 ; My initials
  "MUGWORMGRIBLICK"     ; Wayside School
  "OOSMNSPFRSL"         ; Abbreviation for The Origin of Species
  "OYGEVALT"            ; Oy gevalt
  "PYSZCZYNSKI"         ; Tom Pyszczynski, TMT researcher
  "QLZQQLZUUP"          ; The Emperor Quylthulg in Angband
  "RETSAMTRAFNEGEL"     ; "Legen[dary] fartmaster" (Undertale) backwards
  "SLITHYTOVES"         ; Jabberwocky
  "SPAM"
  "SPISPOPD"            ; http://doom.wikia.com/wiki/SPISPOPD
  "THREEPWOOD"          ; Guybrush Threepwood, of the Monkey Island series
  "TIBYOCSPNLAAD"       ; https://www.reddit.com/r/OutOfTheLoop/comments/1w7ojb
  "TVERSKY"             ; Amos Tversky, JDM researcher
  "UWOTM8"              ; http://knowyourmeme.com/memes/u-wot-m8
  "WOHBANOBONGU"        ; http://cho.cyan.com/rawa/wohba.html
  "X3J13"               ; Common Lisp standardization committee
  "YAGRUMBAGARN"        ; http://www.uesp.net/wiki/Morrowind:Yagrum_Bagarn
  "ZOOMBINI"            ; Zoombinis video-game series
  "ZXKUQYB"])           ; A spell in Ultima III

(def hallucinated-item-strs {
  "a dank meme"
    "Not to be confused with a nice meme."
  "a nice meme"
    "http://niceme.me"
  "a nice meme website"
    "http://nicememe.website"
  "a nice meme website website"
    "http://nicememewebsite.website"
  "a nice meme website website website"
    "http://nicememewebsitewebsite.website"
  "a rare Pepe"
    "Always a solid investment."
  "a bag of Doritos"
    "Oooh, they're mystery-sludge flavor."
  "some goblin mail"
    ; RuneScape
    "But what color is it?"
  "a plain gold ring"
    ; ZAngband's version of the One Ring
    "Where's a volcano when you really need one?"
  "an airhorn"
    "BYOOO BYOOO"
  "the Master Sword"
    "It shoots laser beams if you're at full health. Say, what is your health, anyway?"
  "a Super Mushroom"
    "Maybe you've consumed enough possibly hallucinogenic substances for now."
  "the Eye of Larn"
    ; The roguelike Larn
    "Does advanced dianthroritis lead to coreopsis?"
      ; The Secret Life of Walter Mitty
  "a ring mail"
    ; Starting equipment in Rogue
    "Why is this envelope in the shape of a donut?"
  "the Amulet of Rodney"
    "That isn't what you ordered!"
  "an abandoned quiche"
    ; http://undertale.wikia.com/wiki/Abandoned_Quiche
    "A psychologically damaged spinach egg pie."
  "an annoying dog"
    ; http://undertale.wikia.com/wiki/Annoying_Dog
    "It's clutching a red ball."
  "a piece of the Silver Monkey"
    ; Legends of the Hidden Temple
    "https://www.youtube.com/watch?v=WQvzoY9SaQY\n\nhttps://www.youtube.com/watch?v=PcZawwd7ZZM"
  "a half of a Pendant of Life"
    ; Legends of the Hidden Temple
    "Maybe if you find two, you can make an amulet of life saving."
  "the magic of friendship"
    ; MLP:FIM
    "Multiplayer is not yet implemented."
  "a zap apple"
    ; http://mlp.wikia.com/wiki/Zap_apple
    "This unusual fruit is usually eaten processed into a jam."
  "a MacGuffin"
    "Finally, what you've spent the whole game searching for!"
  "a round tuit"
    "Now you can finally do that one thing."
  "a bitcoin"
    "Magic Internet money."
      ; https://www.reddit.com/r/ads/comments/1q1h40/magic_internet_money
  "a copy of Leisure Suit Larry 4"
    "Who says sequels have to be done sequentially?"
  "a copy of The Secret of Monkey Island disk 22"
    "It's subtitled \"The Secret of the Stump\"."
  "a Larmers™ brand ham medallion"
    ; A running joke on Clickhole
    "Larmers™ brand ham medallions are the perfect snack for any occasion. That's why they've been \"America's Favorite Taste\" since 1928."
  "Heward's handy Hacky Sack®"
    ; A parody of the classic D&D item Heward's handy haversack
    "A footbag that's always at the top of your pocket."
  "a ten-foot pole"
    ; A classic D&D item
    "Never leave home without it."
  "the Head of Vecna"
    ; https://1d4chan.org/wiki/Head_of_Vecna
    "A simple Detect Magic spell can go a long way."
  "the thing that your aunt gave you"
    ; http://everything2.com/title/The+thing+your+aunt+gave+you+which+you+don%27t+know+what+it+is
    "You still don't know what it is."
  "a can of shark-repelling Bat-Spray"
    ; The 1966 Batman movie
    "Not to be confused with the other three Oceanic Repellent Bat-Sprays."
  "a red velvet bag"
    ; Pathways Into Darkness
    "It's lighter on the inside."
  "some jet"
    ; http://fallout.wikia.com/wiki/Jet
    "Ugh, this stuff smells like cow manure."
  "a potion of objet d'étatisation"
    ; A parody of NetHack's potion of object detection
    "Excuse my French."
  "a beetle in a box"
    ; From an argument by Ludwig Wittgenstein
    "If a philosopher's writing is so obtuse that only he can understand what he's trying to say, does a reference to his writings have any place in the language-game at all?"
  "a demijohn full of anteaters"
    "http://arfer.net/anteaters"
  "kitten"
    "You found kitten!"
  "a constitutional amendment"
    "If you think it's hard to get one of these in the US, wait till you see Canada's unanimity formula."
  "the Medicare donut hole"
    "It doesn't look very tasty."
  "the Q document"
    ; https://en.wikipedia.org/wiki/Q_source
    "Looks like Matthew's and Luke's edits were definitely for the better. Just removing Jesus's tedious diatribes about how he didn't want to be a carpenter was a huge improvement."
  "the seventh degree of concentration"
    ; Heartbreak House
    "One difficulty in using psychoactive drugs as some kind of inspiration or means of discovery is that they don't just affect what ideas you come up with; they also affect how you evaluate your ideas—possibly for the worse."
  "Illusens Staff"
    ; http://items.jellyneo.net/item/9399
    "Apostrophes? What are those?"
  "the Nutshack"
    ; http://knowyourmeme.com/memes/the-nutshack-theme
    "It's the Nutshack!"})
; Not currently possible with roguetv.english:
;  "no tea"
;    ; Another weird inventory item from the HHGttG text adventure
;    "You're talking complete nonsense; pull yourself together."
;  "more cowbell"
;    ; https://en.wikipedia.org/wiki/More_cowbell
;    "If you've got a fever…"

(def hallucinated-announcer-names {
  ; Each key is a possible hallucinated name for Tara. The value is
  ; the corresponding name for Bob.
  "Abbot"      "Costello"
  "Akbar"      "Jeff"     ; Life in Hell
  "Bugs"       "Daffy"
  "Cher"       "Sonny"
  "George"     "Harold"   ; Captain Underpants
  "Ginger"     "Fred"
  "Holmes"     "Watson"   ; Sherlock Holmes
  "Kahneman"   "Tversky"
  "Kathie Lee" "Regis"
  "Mitchell"   "Webb"
  "Tom"        "Jerry"
  "Vic"        "Ken"})   ; MXC
