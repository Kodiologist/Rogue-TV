(require kodhy.macros roguetv.macros)

(import
  [kodhy.util [ret]]
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
    (qw they’d         he’d    she’d   it_had  I’d    we’d      you’d   you’d       they’d)])
  ; We only used smart quotes here because "'" can't be in a Hy
  ; identifier. Similarly, we used underscores in place of spaces.
  ; Switch them back.
  (for [row (rest table)]
    (setv (slice row) (amap (.replace (.replace it "’" "'") "_" " ") row)))
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
    (setv (get d (.capitalize k)) (dict (zip
      (.keys v)
      (amap (.capitalize it) (.values v))))))
  d)))

(def genders (frozenset [:male :female :neuter :singular-they]))
(def pronoun-bases (frozenset (.keys -pronoun-d)))

(defn pronoun [base &optional [gender :neuter] [person 3] [plural False]]
; Inflect the pronoun 'base' using 'gender', 'person', and 'plural'.
  (get -pronoun-d base (get
    (if plural [:p1 :p2 :p3] [:s1 :s2 gender])
    (dec person))))

(defn verb [base &optional [gender :neuter] [person 3] [plural False]]
; The 'base' should be in 3rd-person singular form
; (e.g., "is", "was", "does", "did", "swims").
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

  [__init__ (fn [self stem &optional
      plural
      indefinite-singular indefinite-plural
      [always-plural False]
      [proper False]] (block

    (when (instance? NounPhrase stem)
      ; Just clone.
      (setv self.__dict__ stem.__dict__)
      (ret))

    (when proper
      (setv indefinite-singular stem))
    (unless always-plural
      (when (none? indefinite-singular)
        (setv indefinite-singular (.a -inflect stem)))
      (when (none? plural)
        (setv plural (.plural-noun -inflect stem))))
    (when (none? indefinite-plural)
      (setv indefinite-plural (if always-plural stem (.plural-noun -inflect stem))))
    (setv definite-singular (+ (if proper "" "the ") stem))

    (set-self stem plural indefinite-singular indefinite-plural always-plural proper definite-singular)
    None))]])
