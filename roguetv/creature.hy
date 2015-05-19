(require kodhy.macros roguetv.macros)

(import
  [kodhy.util [ret]]
  [roguetv.english [TakesPronouns NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Drawable MapObject]]
  [roguetv.map [room-for?]])

(defclass Creature [Drawable MapObject TakesPronouns] [
  [extant []]
  [char "C"]

  [__init__ (fn [self &optional pos]
    (MapObject.__init__ self pos)
    (setv self.clock-debt-ms 0)
    (self.reset-ice-slipping)
    (.append Creature.extant self)
    None)]

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
        (.take-time self slip-time))))]

  [act (fn [self]
    ; It's this creature's turn to act. Go wild, calling
    ; .take-time as needed.
    (msg "[No .act implemented for {}]" self)
    (.take-time self 1))]])

(defclass Player [Creature] [
  [char "@"]
  [color-bg :yellow]

  [move (fn [self p-to &optional [clobber False]]
    (.move (super Player self) p-to clobber)
    (soil-fov))]])

(import [heidegger.pos [Pos]])

(defclass Cat [Creature] [
  [name (NounPhrase "cat")]
  [char "f"]
  [color-fg :dark-orange]

  [move-chance (/ 1 30)]

  [act (fn [self] (block
    ; Usually just sit there. Occasionally, wander in a random
    ; direction.
    (when (chance self.move-chance)
      (setv neighbors (filt
        (room-for? Creature it)
        (amap (+ self.pos it) Pos.DIR8)))
      (when neighbors
        (setv p-to (pick neighbors))
        (.take-time self (len-taxicab (- p-to self.pos)))
        (.move self p-to)
        (ret)))
    (.take-time self 1)))]])

(defclass Dog [Creature] [
  [name (NounPhrase "dog")]
  [char "d"]
  [color-fg :brown]

  [act (fn [self]
    (.take-time self 1))]])
