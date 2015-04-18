(require kodhy.macros roguetv.macros)

(import
  [roguetv.english :as english]
  [roguetv.types [Drawable MapObject]])

(defclass Creature [Drawable MapObject] [
  [extant []]

  [__init__ (fn [self name char &optional [gender :neuter] [plural False] color-fg color-bg pos]
    (MapObject.__init__ self pos)
    (set-self name char gender plural)
    (set-self-nn color-fg color-bg)
    (setv self.female (= gender :female))
    (setv self.clock-debt-ms 0)
    (.append Creature.extant self)
    None)]

  [__format__ (fn [self formatstr]
    (cond
      [(= formatstr "name")
        self.name]
      [(in formatstr english.pronoun-bases)
        (kwc english.pronoun formatstr
          :gender self.gender
          :plural self.plural)]
      [(.startswith formatstr "v:")
        (kwc english.verb (slice formatstr (len "v:"))
          :gender self.gender
          :plural self.plural)]))]

  [clock-factor 1000]
  [take-time (fn [self duration]
    ; Mark the creature as accumulating 'duration' seconds of
    ; clock debt.
    (+= self.clock-debt-ms (round (* self.clock-factor duration))))]])