(require kodhy.macros)

(import
  unittest
  [roguetv.english [pronoun verb-present]])

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
    (a= pronoun "themself" :gender :female :+plural "themselves"))]

  [test-pronoun-he-hers (fn [self]
    (a= pronoun "he" :gender :male                  "he")
    (a= pronoun "his" :gender :female               "her")
    (a= pronoun "his" :person 1                     "my")
    (a= pronoun "his" :person 1 :+plural            "our")
    (a= pronoun "hers" :person 1 :+plural           "ours")
    (a= pronoun "himself" :gender :singular-they    "themself")
    (a= pronoun "himself" :gender :female           "herself")
    (a= pronoun "himself" :gender :female :+plural  "themselves"))]

  [test-pronoun-cap (fn [self]
    (a= pronoun "They" :gender :female              "She")
    (a= pronoun "Themself" :person 1 :+plural       "Ourselves")
    (a= pronoun "His" :gender :singular-they        "Their")
    (a= pronoun "Hers" :gender :singular-they       "Theirs"))]

  [test-verb-present-be (fn [self]
    (a= verb-present "is" :gender :singular-they    "are")
    (a= verb-present "is" :gender :male             "is")
    (a= verb-present "is" :gender :male :+plural    "are")
    (a= verb-present "is" :person 1                 "am"))]

  [test-verb-present-do (fn [self]
    (a= verb-present "does" :gender :singular-they  "do")
    (a= verb-present "does" :gender :male           "does")
    (a= verb-present "does" :gender :male :+plural  "do")
    (a= verb-present "does" :person 1               "do"))]

  [test-verb-present-go (fn [self]
    (a= verb-present "goes" :gender :singular-they  "go")
    (a= verb-present "goes" :gender :male           "goes")
    (a= verb-present "goes" :gender :male :+plural  "go")
    (a= verb-present "goes" :person 1               "go"))]

  [test-verb-present-swim (fn [self]
    (a= verb-present "swims" :gender :singular-they "swim")
    (a= verb-present "swims" :gender :male          "swims")
    (a= verb-present "swims" :gender :male :+plural "swim")
    (a= verb-present "swims" :person 1              "swim"))]])

(when (= __name__ "__main__")
  (setv suite (.loadTestsFromTestCase (unittest.TestLoader) C))
  (.run (unittest.TextTestRunner) suite))
