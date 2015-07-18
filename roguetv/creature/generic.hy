(require kodhy.macros roguetv.macros)

(import
  [kodhy.util [ret]]
  [roguetv.english [NounPhraseNamed]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Drawable MapObject]]
  [roguetv.map [Tile on-map mget]])

(defclass Creature [Drawable MapObject NounPhraseNamed] [
  [escape-xml-in-np-format True]
  [extant []]
  [char "C"]
  [info-text "[Missing info text]"]

  [can-open-doors False]
  [flying False]

  [__init__ (fn [self &optional pos]
    (MapObject.__init__ self pos)
    (setv self.clock-debt-ms 0)
    (self.reset-ice-slipping)
    (.append Creature.extant self)
    None)]

  [reset-ice-slipping (fn [self]
    (setv self.ice-slip-time 0)
    (setv self.ice-slip-towards None))]

  [information (fn [self]
    (setv s (.format "\n  {} {:a}\n\n{}"
      (.xml-symbol self)
      self
      self.info-text))
    (apply .format [s] (. (type self) __dict__)))]

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

  [wait (fn [self]
    (.take-time self 1))]

  [walk-to (fn [self p-to] (block
    (unless (.bump-into (mget p-to) self)
      (ret False))
    (when (or
        (not (on-map p-to))
        (and (. (Tile.at p-to) blocks-movement)
          (not (and (player? self) (or
            (.has-effect self Passwall)
            G.always-passwall)))))
      (when (and (player? self) (.has-effect self Confusion))
        (msg "You bump into {:the}." (mget p-to))
        (.take-time self G.confusion-bump-time))
      (ret False))
    (setv cr (Creature.at p-to))
    ; The player can push past other creatures, but other creatures
    ; can't push past the player or each other.
    (when cr
      (unless (player? self)
        (ret False))
      (.take-time self G.push-past-monster-time))
    ; Okay, we're clear to move.
    (setv p-from self.pos)
    (.step-out-of (Tile.at p-from) self p-to)
    (.take-time self (/ (dist-taxi p-from p-to) (self.walk-speed)))
    (kwc .move self p-to :+clobber)
    (when cr
      (msg "You push past {:the}." cr)
      (.move cr p-from))
    (when (player? self)
      (rtv display.describe-tile self.pos))
    (.after-step-onto (Tile.at p-to) self p-from)
    True))]

  [has-effect (fn [self effect-cls]
    False)]

  [walk-speed (fn [self]
    ; Return the applicable multiplier for the creature's walking
    ; speed.
    1)]

  [act (fn [self]
    ; It's this creature's turn to act. Go wild, calling
    ; .take-time as needed.
    (msg "[No .act implemented for {}]" self)
    (.wait self))]])

(defclass Effect [object] [
; Despite that this class is in creature.generic instead of
; creature.player, only the player can have effects.

  [__init__ (fn [self expiry]
    (set-self expiry)
    None)]

  [end-msg (fn [self]
    None)]])

(defclass Stink [Effect] [
  [end-msg (fn [self]
    (msg "You smell presentable again."))]])

(defclass Haste [Effect] [
  [end-msg (fn [self]
    (msg "The rush of energy fades."))]])

(defclass Confusion [Effect] [
  [end-msg (fn [self]
    (msg "Your mind clears."))]])

(defclass Strength [Effect] [
  [end-msg (fn [self]
    (msg "You feel like a 98-pound weakling."))]])
      ; Charles Atlas ads

(defclass Passwall [Effect] [
  [end-msg (fn [self]
    (msg "You feel solid again."))]])

(defclass Sleep [Effect] [
  [end-msg (fn [self]
    (msg "You wake up."))]])
