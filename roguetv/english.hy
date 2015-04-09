(require kodhy.macros)

(def -pronoun-d ((fn []
  (setv table [
    [   :singular-they :male   :female :neuter :s1    :p1       :s2      :p2        :p3]
    (qw they           he      she     it      I      we        you      you        they)
    (qw them           him     her     it      me     us        you      your       them)
    (qw their          his     her     its     my     our       your     your       their)
    (qw theirs         his     hers    its     mine   ours      yours    yours      theirs)
    (qw themself       himself herself itself  myself ourselves yourself yourselves themselves)
    (qw they’re        he’s    she’s   it’s    I’m    we’re     you’re   you’re     they’re)])
  ; We only used smart quotes here because "'" can't be in a Hy
  ; identifier. Switch them back.
  (for [row (rest table)]
    (setv (slice row) (amap (.replace it "’" "'") row)))
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

(defn verb-present [base &optional [gender :neuter] [person 3] [plural False]]
; The 'base' should be in 3rd-person singular present-tense form
; (e.g., "is", "does", "swims").
  (when (= gender :singular-they)
    (setv plural True))
  (cond
    [(and (= person 3) (not plural)) base]
    [(= base "is") (cond
      [plural "are"]
      [(= person 2) "are"]
      [(= person 1) "am"])]
    [True (cond
      [(= base "does") "do"]
      [(= base "goes") "go"]
      [True (slice base 0 (dec (len base)))])]))
