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

(def hallucinated-object-strs {
  "item" {
    "a dank meme"
      "Not to be confused with a nice meme."
    "a forced meme"
      "Widely considered the worst kind."
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
      "This isn't what you ordered!"
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
    "a turboencabulator"
      ; https://en.wikipedia.org/wiki/Turboencabulator
      "Careful, this one has only five hydrocoptic marzelvanes."
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
      "It's the Nutshack!"}
  ; Not currently possible with roguetv.english:
  ;  "no tea"
  ;    ; Another weird inventory item from the HhGttG text adventure
  ;    "You're talking complete nonsense; pull yourself together."
  ;  "more cowbell"
  ;    ; https://en.wikipedia.org/wiki/More_cowbell
  ;    "If you've got a fever…"
  "monster" {
    "m:a n00b"
      "how do i shot web?"
        ; http://knowyourmeme.com/memes/how-do-i-shot-web
    "m:a lamer"
      "I bet he save-scums."
    "m:a cheeky scrublord"
      "He only got into Smash Bros. when they added Ryu."
    "m:a tryhard"
      "The coolest guys make it look easy."
    "m:a neckbeard"
      "I WANT TENDIES! REEEE!"
    "m:a hipster"
      "He liked roguelikes before they were cool."
    "m:a cuck"
      "I remember when my high-school English class was reading The Miller's Tale around 2006 and the teacher explained to us what a cuckold was. Little did I suspect that, a decade later, the word \"cuck\" would end up as a central term of art of a new generation of American white nationalists. These are strange times we live in."
    "Barney the Dinosaur"
      "Watch out for its deadly singing attack."
    "m:Cool Cat"
      ; http://knowyourmeme.com/memes/subcultures/cool-cat-saves-the-kids
      "There he is!"
    "a gnome child"
      ; http://knowyourmeme.com/memes/dank-memes
      "Born too late to explore the earth. Born too soon to explore the galaxy. Born just in time to be a dank meme."
    "m:the Wizard of Yendor"
      "\"So thou thought thou couldst elude me, fool.\""
    "the Warden of Yendor"
      "An immoral presence stalks through the dungeon, implacably hunting a poor sap who's just trying to quit while he's ahead."
    "m:Morgoth"
      "His figure is like a light crowning mounted with blackness. No, wait…"
    "m:Missingno"
      "[Missing info text]"
    "m:Sanic"
      "This poorly drawn creature seems to be looking for his shoes."
    "f:Rainbow Dash"
      "A small winged horse known for her speed, agility, guts, style, coolness, awesomeness, and radicalness."
    "a calzone golem"
      ; http://web.archive.org/web/20150714000709/http://www.albinjohnson.com/d&d/resources/downloaded%20adventures/2%20-%20Cooking.pdf
      "Now that's what I call dangerously cheesy."
        ; A slogan of Cheetos.
    "a gazebo"
      ; http://www.comedycorner.org/90.html
      "It looks fierce."
    "a pizza angel"
      ; VeggieTales
      "  Pizza angel, please come to me.\n  Tomato sauce and cheese so gooey.\n  Pizza angel, I'm on my knees.\n  You're my number one pie from Sicily!"
    "a sparkledog"
      "Do not steal."
    "m:Nicolas Cage"
      "How could somebody <b>misfile</b> something? What could be easier? It's all alphabetical. You just <b>put</b> it <b>in</b> the right file! According to alphabetical order! You know: A, B, C, D, E, F, G! H, I, J, K, L, M, N, O, P! Q, R, S, T, U, V! W, X, Y, Z! Huh? That's <b>all</b> you have to <b>do</b>!"
        ; Vampire's Kiss
    "f:Oprah"
      "Everybody in the audience is looking under their seats for their own Amulet of Yendor."
    "m:John Cena"
      "You can, in fact, see him."
    "f:Amelia Earhart"
      "So there she is."
    "f:Lady Cygna"
      ; Loom
      "Yes, Bobbin, this is your mother."
    "m:Chuck Norris"
      "Chuck Norris got to the bonus level in Space Station Silicon Valley.\n\nChuck Norris ascended a foodless atheist survivor.\n\nChuck Norris always goes for the down elevator."
    "m:Scumbag Steve"
      "\"Hey bro, could I borrow the Amulet of Yendor for a sec?\"\n\nDisappears forever."
    "m:Steve Ballmer"
      "I think he's an expert in child development, or something."
        ; http://knowyourmeme.com/memes/steve-ballmer-monkey-dance
    "m:Tommy Oliver"
      "The mysterious Green Ranger."
    "f:Carmen Sandiego"
      "You better watch out. She could steal the whole dungeon."
    "m:Cookie Monster"
      "Imagine how excited and then disappointed he must've been when he saw all the messages born of EU Directive 2009/136/EC."
    "m:a temple guard"
      ; Legends of the Hidden Temple
      "Uh-oh. Do you have any pendants left?"
    "Roko's basilisk"
      "Please do not feed the memes."
    "a Cyberdemon"
      "PROTIP: To defeat the Cyberdemon, shoot at… wait a minute. You don't have a gun. Huh. This could be tricky."
    "a brahmin"
      ; http://fallout.wikia.com/wiki/Brahmin
      "It's double the mooing and double the beef tongue!"
    "a cliff racer"
      ; http://en.uesp.net/wiki/Morrowind:Cliff_Racer
      "They really are everywhere."
    "m:John Kal Hune"
      ; A character from Glenn Seemann's Shadow Keep who is himself
      ; a reference to John Calhoun.
      "\"Flying paper airplanes canst be quite fun. Thou shouldst try it some time.\""
    "m:Dr. Sloth"
      ; Neopets
      "His thesis was about something called immersive advertising."
    "m:W. D. Gaster"
      ; Undertale
      "Insert the Navy SEAL copypasta in Wingdings here."
    "m:Herobrine"
      ; http://knowyourmeme.com/memes/herobrine
      "Don't worry, I removed this character from Rogue TV ages ago."
    "the Shareware Demon"
      ; Exile: Escape from the Pit
      "Your free trial of Rogue TV has ended. To continue past the dreaded Quartz Vein of Crippleware, please send a cashier's check or money order for $14.99 to: The Human Fund, 2880 Broadway, New York, NY, 92880."
        ; The Human Fund is from Seinfeld. The address is that of the diner whose exterior is used for Monk's Café.
    "f:Miss Zarves"
      ; Wayside School
      "She's usually found on dungeon level 19. But there is no dungeon level 19."
    "f:Julie Winters"
      ; The Maxx
      "She knows that there's no such thing as a freelance social worker, right?"
    "the Jersey Devil"
      "Better known as Snooki. Ha, ha, ha, am I right, fellas?"
    "m:Skeleton Man"
      ; The Axis of Awesome
      "  All made up of bone,\n  Shaped like a man.\n  Skeleton feet, skeleton hands.\n  He has calcium strength at his command.\n  Skeleton Man!"
    "a minion"
      "Every day we stray further from God's light."
        ; http://knowyourmeme.com/memes/everyday-we-stray-further-from-god-s-light
    "m:a Starchtrooper"
      ; Why's Poignant Guide to Ruby
      "Remember, <b>synergy</b> means <b>cartoon foxes</b>."
    "a p-zombie"
      "It takes one to know one."
    "a miniature giant space hamster"
      "One imagines it would make for particularly tender spaham."
        ; http://spelljammer.wikia.com/wiki/Spaham
    "an ROUS"
      "Rodents of unusual size? I don't think they exist."
    "m:mr skeltal"
      ; http://knowyourmeme.com/memes/skull-trumpet
      "thank"
    "a wordbank"
      ; http://knowyourmeme.com/memes/wordbank-walrus
      "It stores the words in its tusks."
    "m:Milhouse"
      ; http://knowyourmeme.com/memes/milhouse-is-not-a-meme
      "He's a meme, and that's final."
    "m:( ͡° ͜ʖ ͡°)"
      ; http://knowyourmeme.com/memes/lenny-face
      "( ͡° ͜ʖ ͡°)"
    "m:Roland Moralheart"
      "Hey, did you ever notice that \"Disraeli\" is an anagram of \"I lead, sir\"?"
    "m:Kodi"
      "Hey, I'm not supposed to be here! I'm a busy man!"}})
  ; Not currently possible with roguetv.english:
  ;  "f:your mother"
  ;    "A being of legendary corpulence."

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

(def hallucinated-push-past-verbs [
  "rek"
  "no-scope"
  "quickscope"
  "MLG"
  "pwn"
  "diss"
  "clap back at"
  "dominate"
  "wavedash past"
  "blaze"
  "friendzone"
  "faze"
  "meme"
  "accidentally"
  "You set up {:the} the bomb."])

(def hallucinated-item-verbs [
  "bamboozle"
  "be when you grow up"
  "censor"
  "congratulate"
  "cough up"
  "debug"
  "defenestrate"
  "disappoint"
  "downsize"
  "embarrass"
  "embezzle"
  "endorse"
  "enjoy"  ; An action in the HhGttG text adventure
  "enshrine"
  "excommunicate"
  "fight"
  "finagle"
  "fold, spindle, or mutilate"
  "fondly regard"  ; http://mspaintadventures.wikia.com/wiki/Fondly_regard_creation
  "gank"
  "gently caress"  ; http://www.urbandictionary.com/define.php?term=Gently%20Caress
  "inflate"
  "lampoon"
  "like on Facebook"
  "misunderstand"
  "patent"
  "politicize"
  "polymorph into"
  "psychoanalyze"
  "reassure"
  "redecorate"
  "reify"
  "repost"
  "romanticize"
  "sacrifice to Moloch"
  "shave"
  "slander"
  "smuggle"
  "steal the identity of"
  "stick up your nose"
  "trisect"
  "tweet"
  "unravel"
  "upcycle"
  "vote for"])
