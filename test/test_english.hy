(require kodhy.macros)

(import
  unittest
  [roguetv.english [pronoun verb]])

(defmacro a= [&rest args]
  (setv [x v] [(butlast args) (get args -1)])
  `(.assertEqual self (kwc ~@x) ~v))

(defclass C [unittest.TestCase] [

  [test-pronoun-they (fn [self]
    (a= pronoun "they" :gender :male                "he")
    (a= pronoun "their" :gender :female             "her")
    (a= pronoun "their" :person 1                   "my")
    (a= pronoun "their" :person 1 :+plural          "our")
    (a= pronoun "theirs" :person 1 :+plural         "ours")
    (a= pronoun "themself" :gender :singular-they   "themself")
    (a= pronoun "themself" :gender :female          "herself")
    (a= pronoun "themself" :gender :female :+plural "themselves")
    (a= pronoun "they're" :person 2                 "you're")
    (a= pronoun "they've" :person 2                 "you've")
    (a= pronoun "they'll" :gender :female           "she'll")
    (a= pronoun "they'd"  :gender :female           "she'd"))]

  [test-pronoun-he-hers (fn [self]
    (a= pronoun "he" :gender :male                  "he")
    (a= pronoun "his" :gender :female               "her")
    (a= pronoun "his" :person 1                     "my")
    (a= pronoun "his" :person 1 :+plural            "our")
    (a= pronoun "hers" :person 1 :+plural           "ours")
    (a= pronoun "himself" :gender :singular-they    "themself")
    (a= pronoun "himself" :gender :female           "herself")
    (a= pronoun "himself" :gender :female :+plural  "themselves")
    (a= pronoun "he's"  :person 2                   "you're")
    (a= pronoun "he'll" :gender :female             "she'll")
    (a= pronoun "he'd" :gender :female              "she'd"))]

  [test-pronoun-cap (fn [self]
    (a= pronoun "They" :gender :female              "She")
    (a= pronoun "Themself" :person 1 :+plural       "Ourselves")
    (a= pronoun "His" :gender :singular-they        "Their")
    (a= pronoun "Hers" :gender :singular-they       "Theirs"))]

  [test-verb-be-present (fn [self]
    (a= verb "is" :gender :singular-they    "are")
    (a= verb "is" :gender :male             "is")
    (a= verb "is" :gender :male :+plural    "are")
    (a= verb "is" :person 1                 "am"))]

  [test-verb-be-past (fn [self]
    (a= verb "was" :gender :singular-they    "were")
    (a= verb "was" :gender :male             "was")
    (a= verb "was" :gender :male :+plural    "were")
    (a= verb "was" :person 1                 "was"))]

  [test-verb-present-have (fn [self]
    (a= verb "has" :gender :singular-they  "have")
    (a= verb "has" :gender :male           "has")
    (a= verb "has" :gender :male :+plural  "have")
    (a= verb "has" :person 1               "have"))]

  [test-verb-present-swim (fn [self]
    (a= verb "swims" :gender :singular-they "swim")
    (a= verb "swims" :gender :male          "swims")
    (a= verb "swims" :gender :male :+plural "swim")
    (a= verb "swims" :person 1              "swim"))]

  [test-verb-other (fn [self]
    (a= verb "buzzes" :+plural              "buzz")
    (a= verb "catches" :+plural             "catch")
    (a= verb "lurches" :+plural             "lurch")
    (a= verb "embargoes" :+plural           "embargo")
    (a= verb "does" :+plural                "do")
    (a= verb "cries" :+plural               "cry"))]])

(when (= __name__ "__main__")
  (setv suite (.loadTestsFromTestCase (unittest.TestLoader) C))
  (.run (unittest.TextTestRunner) suite))
