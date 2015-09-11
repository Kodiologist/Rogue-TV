(require kodhy.macros roguetv.macros)

(import
  [kodhy.util [ret]]
  [roguetv.english [NounPhraseNamed]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Drawable MapObject Scheduled]]
  [roguetv.map [Tile on-map mget]])

(defclass Creature [Drawable MapObject Scheduled NounPhraseNamed] [
  [escape-xml-in-np-format True]
  [char "C"]
  [info-text "[Missing info text]"]

  [can-open-doors False]
  [flying False]
  [slime-immune False]
  [web-immune False]

  [__init__ (fn [self &optional pos]
    (MapObject.__init__ self pos)
    (Scheduled.__init__ self)
    (self.schedule)
    (self.reset-ice-slipping)
    None)]

  [reset-ice-slipping (fn [self]
    (setv self.ice-slip-time 0)
    (setv self.ice-slip-towards None))]

  [ice-immune (fn [self]
    False)]

  [information (fn [self]
    (.format "\n  {} {:a}\n\n{}"
      (.xml-symbol self)
      self
      (apply .format [self.info-text] (. (type self) __dict__))))]

  [take-time (fn [self duration]
    (.take-time Scheduled self duration)
    (when (and duration self.ice-slip-time)
      ; The creature takes some extra time slipping.
      (setv slip-time self.ice-slip-time)
      (msgp self "You take a moment to steady yourself on the ice.")
      (self.reset-ice-slipping)
      (.take-time self slip-time)))]

  [walk-to (fn [self p-to] (block
    (unless (.bump-into (mget p-to) self)
      (ret False))
    (when (player? self) (whenn (afind-or it.superheavy G.inventory)
      (msg "You can't move an inch so long as you're clinging to {:the}." it)
      (ret False)))
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
    1)]])

(defclass Effect [object] [
; Despite that this class is in creature.generic instead of
; creature.player, only the player can have effects.

  [status "???"]
    ; Text shown in the status bar.
  [end-msg None]

  [__init__ (fn [self expiry]
    (set-self expiry)
    None)]])

(defcls Stink [Effect]
  status "PU"
  end-msg "You smell presentable again.")

(defcls Haste [Effect]
  status "5SE"
  end-msg "The rush of energy fades.")

(defcls Confusion [Effect]
  status "Conf"
  end-msg "Your mind clears.")

(defcls Strength [Effect]
  status "Str"
  end-msg "You feel like a 98-pound weakling.")
    ; Charles Atlas ads

(defcls Passwall [Effect]
  status "Pass"
  end-msg "You feel solid again.")

(defcls Sleep [Effect]
  status "Zzz"
  end-msg "You wake up.")
