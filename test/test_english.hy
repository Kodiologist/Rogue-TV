(import
  unittest
  [kodhy.util [T F]]
  [roguetv.english [pronoun verb NounPhrase]])

(defmacro a= [&rest args]
  (setv [x v] [(butlast args) (get args -1)])
  `(.assertEqual self (~@x) ~v))

(defn npf [np form] (cond
  [(= form "ds")
    (.format "{:The} {:v:is} destroyed." np np)]
  [(= form "dp")
    (.format "{:p-The} {:p-v:is} destroyed." np np)]
  ; "This", etc., are not currently implemented.
;  [(= form "t1s")
;    (.format "{:This} {:v:is} destroyed." np np)]
;  [(= form "t1p")
;    (.format "{:These} {:p-v:is} destroyed." np np)]
;  [(= form "t2s")
;    (.format "{:That} {:v:is} destroyed." np np)]
;  [(= form "t2p")
;    (.format "{:Those} {:p-v:is} destroyed." np np)]
  [(= form "is")
    (.format "{:A} {:v:is} here." np np)]
  [(= form "ip")
    (.format "{:Some} {:p-v:is} here." np np)]
  [(= form "p")
    (.format "{:Your} {:v:is} destroyed." np np)]
  [(= form "n")
    (.format "You won 2 {:num}." np)]))

(defmacro npf= [np form answer]
  `(.assertEqual self (npf ~np ~(name form)) ~answer))

(defclass C [unittest.TestCase] [

  test-pronoun-they (fn [self]
    (a= pronoun "they" :gender :male                 "he")
    (a= pronoun "their" :gender :female              "her")
    (a= pronoun "their" :person 1                    "my")
    (a= pronoun "their" :person 1 :plural T          "our")
    (a= pronoun "theirs" :person 1 :plural T         "ours")
    (a= pronoun "themself" :gender :singular-they    "themself")
    (a= pronoun "themself" :gender :female           "herself")
    (a= pronoun "themself" :gender :female :plural T "themselves")
    (a= pronoun "they're" :person 2                  "you're")
    (a= pronoun "they've" :person 2                  "you've")
    (a= pronoun "they'll" :gender :female            "she'll")
    (a= pronoun "they'd"  :gender :female            "she'd"))

  test-pronoun-he-hers (fn [self]
    (a= pronoun "he" :gender :male                  "he")
    (a= pronoun "his" :gender :female               "her")
    (a= pronoun "his" :person 1                     "my")
    (a= pronoun "his" :person 1 :plural T           "our")
    (a= pronoun "hers" :person 1 :plural T          "ours")
    (a= pronoun "himself" :gender :singular-they    "themself")
    (a= pronoun "himself" :gender :female           "herself")
    (a= pronoun "himself" :gender :female :plural T "themselves")
    (a= pronoun "he's"  :person 2                   "you're")
    (a= pronoun "he'll" :gender :female             "she'll")
    (a= pronoun "he'd" :gender :female              "she'd"))

  test-pronoun-cap (fn [self]
    (a= pronoun "They" :gender :female              "She")
    (a= pronoun "Themself" :person 1 :plural T      "Ourselves")
    (a= pronoun "His" :gender :singular-they        "Their")
    (a= pronoun "Hers" :gender :singular-they       "Theirs"))

  test-verb-be-present (fn [self]
    (a= verb "is" :gender :male              "is")
    (a= verb "is" :gender :male :plural T    "are")
    (a= verb "is" :person 1                  "am"))

  test-verb-be-past (fn [self]
    (a= verb "was" :gender :male             "was")
    (a= verb "was" :gender :male :plural T   "were")
    (a= verb "was" :person 1                 "was"))

  test-verb-present-have (fn [self]
    (a= verb "has" :gender :male             "has")
    (a= verb "has" :gender :male :plural T   "have")
    (a= verb "has" :person 1                 "have"))

  test-verb-present-swim (fn [self]
    (a= verb "swims" :gender :male           "swims")
    (a= verb "swims" :gender :male :plural T "swim")
    (a= verb "swims" :person 1               "swim"))

  test-verb-other (fn [self]
    (a= verb "buzzes" :plural T              "buzz")
    (a= verb "catches" :plural T             "catch")
    (a= verb "lurches" :plural T             "lurch")
    (a= verb "embargoes" :plural T           "embargo")
    (a= verb "does" :plural T                "do")
    (a= verb "cries" :plural T               "cry"))

  test-npformat-count (fn [self]
    (setv np (NounPhrase "stapler"))
    (npf= np :ds   "The stapler is destroyed.")
    (npf= np :dp   "The staplers are destroyed.")
    (npf= np :is   "A stapler is here.")
    (npf= np :ip   "Some staplers are here.")
    (npf= np :p    "Your stapler is destroyed.")
    (npf= np :n    "You won 2 staplers.")

    ; Manually setting the article.
    (setv np (NounPhrase "stapler" :article "an"))
    (npf= np :is   "An stapler is here."))

  test-npformat-count-irregplural (fn [self]
    ; Automatically detectable by `inflect`.
    (setv np (NounPhrase "mouse"))
    (npf= np :ds   "The mouse is destroyed.")
    (npf= np :dp   "The mice are destroyed.")
;    (npf= np :t1s  "This mouse is destroyed.")
;    (npf= np :t1p  "These mice are destroyed.")
;    (npf= np :t2s  "That mouse is destroyed.")
;    (npf= np :t2p  "Those mice are destroyed.")
    (npf= np :is   "A mouse is here.")
    (npf= np :ip   "Some mice are here.")
    (npf= np :p    "Your mouse is destroyed.")
    (npf= np :n    "You won 2 mice.")

    ; Not automatically detectable.
    (setv np (NounPhrase "box" :plural "boxen"))
    (npf= np :ds   "The box is destroyed.")
    (npf= np :dp   "The boxen are destroyed.")
    (npf= np :is   "A box is here.")
    (npf= np :ip   "Some boxen are here.")
    (npf= np :p    "Your box is destroyed.")
    (npf= np :n    "You won 2 boxen."))

  test-npformat-mass (fn [self]
    (setv np (NounPhrase "peanut butter" :mass T :unit "globs"))
    (npf= np :ds   "The peanut butter is destroyed.")
    (npf= np :dp   "The peanut butter is destroyed.")
;    (npf= np :t1s  "This peanut butter is destroyed.")
;    (npf= np :t1p  "This peanut butter is destroyed.")
;    (npf= np :t2s  "That peanut butter is destroyed.")
;    (npf= np :t2p  "That peanut butter is destroyed.")
    (npf= np :is   "Some peanut butter is here.")
    (npf= np :ip   "Some peanut butter is here.")
    (npf= np :p    "Your peanut butter is destroyed.")
    (npf= np :n    "You won 2 globs of peanut butter."))

  test-npformat-pluraletantum (fn [self]
    (setv np (NounPhrase "pants" :always-plural T :unit "pairs"))
    (npf= np :ds   "The pants are destroyed.")
    (npf= np :dp   "The pants are destroyed.")
    (npf= np :is   "Some pants are here.")
    (npf= np :ip   "Some pants are here.")
    (npf= np :p    "Your pants are destroyed.")
    (npf= np :n    "You won 2 pairs of pants.")

    ; Nouns that are nominally count nouns, but are always
    ; regarded in quantity by the game, may be treated the same
    ; as real plurale tantum.
    (setv np (NounPhrase "sunflower seeds" :always-plural T :unit "handfuls"))
    (npf= np :ds   "The sunflower seeds are destroyed.")
    (npf= np :dp   "The sunflower seeds are destroyed.")
    (npf= np :is   "Some sunflower seeds are here.")
    (npf= np :ip   "Some sunflower seeds are here.")
    (npf= np :p    "Your sunflower seeds are destroyed.")
    (npf= np :n    "You won 2 handfuls of sunflower seeds."))

  test-npformat-proper-singular (fn [self]
    (setv np (NounPhrase "Stormbringer" :bare-proper T))
    (npf= np :ds   "Stormbringer is destroyed.")
    (npf= np :dp   "The Stormbringers are destroyed.")
;    (npf= np :t1s  "Stormbringer is destroyed.")
;    (npf= np :t1p  "These Stormbringers are destroyed.")
;    (npf= np :t2s  "Stormbringer is destroyed.")
;    (npf= np :t2p  "Those Stormbringers are destroyed.")
    (npf= np :is   "Stormbringer is here.")
    (npf= np :ip   "Some Stormbringers are here.")
    (npf= np :p    "Stormbringer is destroyed.")
    (npf= np :n    "You won 2 Stormbringers."))

  test-npformat-theproper-singular (fn [self]
    (setv np (NounPhrase "Black Scythe" :the-proper T))
    (npf= np :ds   "The Black Scythe is destroyed.")
    (npf= np :dp   "The Black Scythes are destroyed.")
;    (npf= np :t1s  "The Black Scythe is destroyed.")
;    (npf= np :t1p  "The Black Scythes are destroyed.")
;    (npf= np :t2s  "The Black Scythe is destroyed.")
;    (npf= np :t2p  "The Black Scythes are destroyed.")
    (npf= np :is   "The Black Scythe is here.")
    (npf= np :ip   "Some Black Scythes are here.")
    (npf= np :p    "The Black Scythe is destroyed.")
    (npf= np :n    "You won 2 Black Scythes."))

  ; English grammar does not seem to allow a plural direct
  ; equivalent of "Stormbringer". Here, "Santa's" is a determiner,
  ; not just part of a name. "Your Santa's Pants" is arguably not
  ; grammatical.
  ; "Santa's Pants are destroyed.", etc.

  test-npformat-theproper-plural (fn [self]
    (setv np (NounPhrase "Eyes of the Overworld" :the-proper T :always-plural T :unit "pairs"))
    (npf= np :ds   "The Eyes of the Overworld are destroyed.")
    (npf= np :dp   "The pairs of the Eyes of the Overworld are destroyed.")
;    (npf= np :t1s  "The Eyes of the Overworld are destroyed.")
;    (npf= np :t1p  "These pairs of the Eyes of the Overworld are destroyed.")
;    (npf= np :t2s  "The Eyes of the Overworld are destroyed.")
;    (npf= np :t2p  "Those pairs of the Eyes of the Overworld are destroyed.")
    (npf= np :is   "The Eyes of the Overworld are here.")
    (npf= np :ip   "Some pairs of the Eyes of the Overworld are here.")
    (npf= np :p    "The Eyes of the Overworld are destroyed.")
    (npf= np :n    "You won 2 pairs of the Eyes of the Overworld."))

  test-npformat-stem-with-unit (fn [self]
    (defn f [x y] (a= .format "{:p-the}" (NounPhrase x) (+ "the " y)))
    (f "slice of cake"      "slices of cake")
    (f "mug of coffee"      "mugs of coffee")
    (f "can of Coke"        "cans of Coke")
    (f "box of Froot Loops" "boxes of Froot Loops"))])

(when (= __name__ "__main__")
  (setv suite (.loadTestsFromTestCase (unittest.TestLoader) C))
  (.run (unittest.TextTestRunner) suite))
