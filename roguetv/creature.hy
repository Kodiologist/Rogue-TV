(require kodhy.macros roguetv.macros)

(import
  [roguetv.english :as english]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Drawable MapObject]])

(defclass Creature [Drawable MapObject english.NounPhraseNamed] [
  [extant []]
  [char "C"]
  [plural False]

  [__init__ (fn [self &optional [gender :neuter] pos]
    (MapObject.__init__ self pos)
    (set-self gender)
    (setv self.female (= gender :female))
    (setv self.clock-debt-ms 0)
    (self.reset-ice-slipping)
    (.append Creature.extant self)
    None)]

  [__format__ (fn [self formatstr]
    (or (english.NounPhraseNamed.__format__ self formatstr) (cond
      [(in formatstr english.pronoun-bases)
        (kwc english.pronoun formatstr
          :gender self.gender
          :plural self.plural)]
      [(.startswith formatstr "v:")
        (kwc english.verb (slice formatstr (len "v:"))
          :gender self.gender
          :plural self.plural)])))]

  [reset-ice-slipping (fn [self]
    (setv self.ice-slip-time 0)
    (setv self.ice-slip-towards None))]

  [clock-factor 1000]
  [take-time (fn [self duration]
    ; Mark the creature as accumulating 'duration' seconds of
    ; clock debt.
    (when duration
      (+= self.clock-debt-ms (round (* self.clock-factor duration)))
      (when self.ice-slip-time
        ; The creature takes some extra time slipping.
        (setv slip-time self.ice-slip-time)
        (msgp self "You take a moment to steady yourself on the ice.")
        (self.reset-ice-slipping)
        (.take-time self slip-time))))]])

(defclass Player [Creature] [
  [char "@"]
  [color-bg :yellow]

  [move (fn [self &rest args]
    (apply .move (+ (, (super Player self)) args))
    (soil-fov))]])
