(require [kodhy.macros [amap afind-or block qw]] [roguetv.macros [*]])

(import
  xml.sax.saxutils
  [kodhy.util [ret ucfirst]]
  inflect)

(def -inflect (inflect.engine))

(def -pronoun-d ((fn []
  (setv table [
    [   :singular-they :male   :female :neuter :s1    :p1       :s2      :p2        :p3]
    (qw they           he      she     it      I      we        you      you        they)
    (qw them           him     her     it      me     us        you      your       them)
    (qw their          his     her     its     my     our       your     your       their)
    (qw theirs         his     hers    its     mine   ours      yours    yours      theirs)
    (qw themself       himself herself itself  myself ourselves yourself yourselves themselves)
    (qw they’re        he’s    she’s   it’s    I’m    we’re     you’re   you’re     they’re)
    (qw they’ll        he’ll   she’ll  it’ll   I’ll   we’ll     you’ll   you’ll     they’ll)
    (qw they’ve        he’s    she’s   it’s    I’ve   we’ve     you’ve   you’ve     they’ve)
    (qw they’d         he’d    she’d   it_had  I’d    we’d      you’d    you’d      they’d)])
  ; We only used smart quotes here because "'" can't be in a Hy
  ; identifier. Similarly, we used underscores in place of spaces.
  ; Switch them back.
  (for [row (rest table)]
    (setv (cut row) (amap (.replace (.replace it "’" "'") "_" " ") row)))
  ; Set 'd' to a dictionary mapping the :singular-they forms
  ; to dictionaries of all forms for that part of speech.
  (setv cols (first table))
  (setv d (dict (amap
    (, (first it) (dict (zip cols it)))
    (rest table))))
  ; Also allow using masculine pronouns as keys into 'd'.
  (for [v (list (.values d))]
    (setv (get d (get v :male)) v))
  ; "His" is ambiguous. Handle it by making "his" always maps to
  ; "their" and providing "hers" for "theirs".
  (setv (get d "his") (get d "their"))
  (setv (get d "hers") (get d "theirs"))
  ; "He's" is also ambiguous. Handle it by making "he's" always
  ; maps to "they're". This time, the feminine form doesn't help,
  ; so you must use "they've" if you want "I've" etc.
  (setv (get d "he's") (get d "they're"))
  ; Finally, create capitalized forms of everything.
  (for [[k v] (list (.items d))]
    (setv (get d (ucfirst k)) (dict (zip
      (.keys v)
      (amap (ucfirst it) (.values v))))))
  d)))

(def genders (, :male :female :neuter :singular-they))
(def pronoun-bases (frozenset (.keys -pronoun-d)))

(defn pronoun [base &optional [gender :neuter] [person 3] [plural False]]
; Inflect the pronoun 'base' using 'gender', 'person', and 'plural'.
  (get -pronoun-d base (get
    (if plural [:p1 :p2 :p3] [:s1 :s2 gender])
    (dec person))))

(defn verb [base &optional [gender :neuter] [person 3] [plural False]]
; The 'base' should be in 3rd-person singular form
; (e.g., "is", "was", "does", "did", "swims").
  (when (= gender :singular-they)
    ; The pronoun requires a plural form, but the original noun
    ; might be singular. We can't tell which is which.
    (raise (ValueError "Inflecting a verb for a :singular-they subject is ambiguous")))
  (cond
    [(and (= base "is") (= person 1) (not plural))
      "am"]
    [(and (= base "was") (= person 1) (not plural))
      "was"]
    [(and (= person 3) (!= gender :singular-they) (not plural))
      base]
    [True
      (.plural-verb -inflect base)]))

(defclass NounPhrase [object] [

  __init__ (fn [self stem &optional
      plural
      [gender :neuter]
      article
      [mass False]
      [always-plural False]
      unit
      [bare-proper False]
      [the-proper False]] (block

    (when (instance? NounPhrase stem)
      ; Just clone.
      (setv self.__dict__ stem.__dict__)
      (ret))

    (when (or
        (not (in gender genders))
        (and (or mass always-plural) (not unit))
        (and unit (not (or mass always-plural)))
        (and always-plural plural)
        (and always-plural bare-proper))
      (raise (ValueError (+ "Bad NounPhrase parameters: " stem))))

    (setv united (when unit
      (.format "{} of {}{}" unit (if the-proper "the " "") stem)))
    (setv pluralized (cond
      [(or always-plural mass)
        (if the-proper united stem)]
      [plural
        plural]
      [True
        (.plural-noun -inflect stem)]))

    (setv definite-singular
      (if bare-proper stem (+ "the " stem)))
    (setv definite-plural
      (+ "the " pluralized))
    (setv indefinite-singular
      (cond
        [(or bare-proper the-proper)
          definite-singular]
        [(or mass always-plural)
          (+ "some " stem)]
        [article
          (+ article " " stem)]
        [True
          (.a -inflect stem)]))
    (setv indefinite-plural
      (+ "some " pluralized))
    (setv your
      (if (or bare-proper the-proper) definite-singular (+ "your " stem)))
    (setv count
      (if unit united pluralized))

    (set-self
      stem gender mass always-plural
      definite-singular definite-plural indefinite-singular indefinite-plural your count)
    None))

  __format__ (fn [self formatstr]
    (setv upper (not (none? (afind-or (.isupper it) formatstr))))
    (setv formatstr (.lower formatstr))
    ((if upper ucfirst identity) (cond
      [(in formatstr pronoun-bases)
        (pronoun formatstr
          :gender self.gender
          :plural self.always-plural)]
      [(.startswith formatstr "v:")
        (verb (cut formatstr (len "v:"))
          :gender self.gender
          :plural self.always-plural)]
      [(.startswith formatstr "p-v:")
        (verb (cut formatstr (len "p-v:"))
          :gender self.gender
          :plural (not self.mass))]
      [True
        (get
          {
            ""      self.stem
            "the"   self.definite-singular
            "p-the" self.definite-plural
            "a"     self.indefinite-singular
            "some"  self.indefinite-plural
            "your"  self.your
            "num"   self.count}
          formatstr)])))

  female (fn [self]
    (= self.gender :female))])

(defclass NounPhraseNamed [object] [
  name None
  escape-xml-in-np-format False

  escape (classmethod (fn [self s]
    (if (and s self.escape-xml-in-np-format)
      (xml.sax.saxutils.escape s)
      s)))

  __format__ (fn [self formatstr]
    (.escape self (.__format__ self.name formatstr)))])

(defn english-list [l]
  (.join -inflect l))
