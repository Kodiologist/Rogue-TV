(require kodhy.macros roguetv.macros)

(import
  [random [randint expovariate]]
  [kodhy.util [ret]]
  [roguetv.english [NounPhrase NounPhraseNamed]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [y-or-n]]
  [roguetv.types [Drawable MapObject Scheduled]])

(defclass Tile [Drawable MapObject NounPhraseNamed Scheduled] [
  [escape-xml-in-np-format True]
  [info-text "[Missing info text]"]
  [blocks-movement False]
  [blocks-sight False]
  [unpleasant False]
    ; `unpleasant` is a flag meaning that monsters tend not to
    ; want to be on this kind of tile.
  [smooth False]

  [information (fn [self]
    (.format "\n  {} {:a}\n\n{}"
      (.xml-symbol self)
      self
      (apply .format [self.info-text] (. (type self) __dict__))))]

  [use-tile (fn [self cr]
    ; A creature has tried to do something with this tile. (For
    ; the player, that would be using the command :use-tile.)
    ;
    ; The default implementaton does nothing.
    (msgp cr "There's nothing special you can do at this tile."))]

  [use-item-while-here (fn [self]
    ; The player has tried to use (i.e., apply or drop) an item
    ; while standing on this tile. Return False to halt further
    ; processing of the action, and True to continue.
    True)]

  [bump-into (fn [self cr]
    ; A creature has tried to step onto this tile. Return
    ; False to halt further processing of the step (in particular,
    ; the creature is not moved), and True to continue.
    True)]

  [after-step-onto (fn [self cr p-from]
    ; A creature has just finished stepping onto this tile.
    None)]

  [step-out-of (fn [self cr p-to]
    ; A creature is stepping out of this tile.
    None)]])

(defclass LevelBoundary [NounPhraseNamed] [
  ; This object is not a real tile. Rather, it is used as a flag
  ; for the lack of a tile when you use `mget` rather than `Tile.at`.
  [name (kwc NounPhrase "level boundary" :+mass :unit "sections")]
  [blocks-movement True]
  [blocks-sight True]
  [bump-into (fn [self cr] True)]])
(setv LevelBoundary (LevelBoundary))

(defn mset [pos tile &optional [fov-adjust True]]
  (when (and fov-adjust (!= tile.blocks-sight (. (Tile.at pos) blocks-sight)))
    (soil-fov))
  (whenn (Tile.at pos)
    (.destroy it))
  (kwc .move tile pos :+clobber))

(defn mget [pos]
  (if (on-map pos)
    (Tile.at pos)
    LevelBoundary))

(defn on-map [pos]
  (and (<= 0 pos.x (dec G.map-width)) (<= 0 pos.y (dec G.map-height))))

(defn room-for? [mo-class pos]
  (and
    (on-map pos)
    (not (. (Tile.at pos) blocks-movement))
    (not (.at mo-class pos))))

(defn ray-taxi [
    start     ; Pos
    direction ; Pos
    length    ; int
    &optional
    [include-off-map False]]
  ; Produces a list of Pos. `direction` should be in Pos.DIR8.
  ; If `direction` is orthogonal, you get `length` elements.
  ; Otherwise, you get `length` // 2 elements.
  (setv l [start])
  (for [_ (range (dec (if (in direction Pos.DIAGS) (// length 2) length)))]
    (setv p (+ (get l -1) direction))
    (unless (or include-off-map (on-map p))
      (break))
    (.append l p))
  l)

(defn disc-taxi [
    center  ; Pos
    radius] ; int
  ; Produces a list of Pos. In taxicab geometry, a disc is a square
  ; sitting on one of its vertices.
  (list-comp (+ center (Pos dx dy))
    [
      dx (seq (- radius) radius)
      dy (seq (- (abs dx) radius) (- radius (abs dx)))]
    (on-map (+ center (Pos dx dy)))))

(defn circ-taxi [
    center  ; Pos
    radius] ; int
  ; Like disc-taxi, but with only the boundary points.
  (list-comp (+ center (Pos dx dy))
    [
      dx (seq (- radius) radius)
      dy [(- (abs dx) radius) (- radius (abs dx))]]
    (on-map (+ center (Pos dx dy)))))

(defn in-los? [eye target] (block
  (for [p (slice (line-bresen eye target) 1)]
    (when (. (mget p) blocks-sight)
      (ret False)))
  True))

(defclass Floor [Tile] [
  [name (kwc NounPhrase "ordinary floor" :+mass :unit "tiles")]
  [char "."]
  [info-text "Just what it says on the tin."]

  [smooth True]])

(defclass Wall [Tile] [
  [name (NounPhrase "wall")]
  [char "#"]
  [color-bg G.fg-color]
  [info-text (.join "\n" [
    "A common and persistent obstacle to scooping up all the prizes on the level and hopping into the down elevator."
    ""
    "  This man, with lime and rough-cast, doth present"
    "  Wall, that vile Wall which did these lovers sunder;"
    "  And through Wall's chink, poor souls, they are content"
    "  To whisper, at the which let no man wonder."])]
      ; A Midsummer Night's Dream, 5.1.131–134
  [blocks-movement True]
  [blocks-sight True]])

(defclass Elevator [Tile] [
  [color-bg :green]
  [smooth True]])

(defclass UpElevator [Elevator] [
  [name (NounPhrase "elevator going up")]
  [char "<"]
  [info-text "Taking the elevator back up will immediately end your game of Rogue TV, but you'll be able to keep whatever winnings you're carrying."]

  [use-tile (fn [self cr]
    (when (player? cr)
      (msg :tara "Beware, there will be no return!")
      (when (y-or-n "Take the elevator up?" :+require-uppercase)
        (setv G.endgame :used-up-elevator))))]])

(defn upelevator-pos [] (block
  (for [l Tile.omap]
    (for [t l]
      (when (instance? UpElevator t)
        (ret t.pos))))))

(defclass DownElevator [Elevator] [
  [name (NounPhrase "elevator going down")]
  [char ">"]
  [info-text "This elevator leads to a new, unexplored level. The time limit will be reset, but you won't be able to return to this level, so make sure you're carrying whatever you intend to keep."]

  [use-tile (fn [self cr]
    (when (player? cr)
      (if (= G.dungeon-level G.max-dungeon-level)
        (if (afind-or (instance? (get G.itypes "aoy") it) G.inventory)
          (do
            (msg :aud "goes wild! You won the game!")
            (setv G.endgame :won))
          (msg :tara "Sorry, {p}, you'll need the Amulet of Yendor to finish the game."))
        (do
          (when G.autosave
            (rtv saves.write-save-file G.save-file-path))
          (when (<= (- G.time-limit G.current-time) G.super-low-time-threshold)
            ; The audience was counting down when the player got
            ; to the down elevator.
            (msg :aud "cheers. You made it!"))
          (+= G.dungeon-level 1)
          (rtv mapgen.reset-level)
          (msg :tara
            (if (= G.dungeon-level G.max-dungeon-level)
              "{p} has reached level {}, the final level. It's {} by {} squares. {p} must now find the mystical Amulet of Yendor and take the final down elevator to win Rogue TV!"
              "And {p:he's} on to level {}. It spans {} by {} squares.")
            (inc G.dungeon-level) G.map-width G.map-height)))))]])

(defclass Door [Tile] [
  [color-fg :brown]
  [info-text "Rogue TV has obtained only the most rotten and ill-fitting of doors to block your progress through the level. They're unlocked, but heaving them open will take some time. And once open, they'll swing shut after a while."]])

(defclass ClosedDoor [Door] [
  [name (NounPhrase "closed door")]
  [char "+"]

  [blocks-movement True]
  [blocks-sight True]

  [open-time (fn [self]
    (+ 2 G.dungeon-level (randint 1 8)))]

  [bump-into (fn [self cr] (block
    (unless cr.can-open-doors
      (ret True))
    (setv open-time (.open-time self))
    (if (.has-effect cr (rtv-get creature.Strength))
      (msgp cr "You effortlessly kick the door open.")
      (do
        (msgp cr "You open the old door after a struggle.")
        (cr.take-time open-time)))
    (mset self.pos (OpenDoor open-time))
    False))]])

(defclass OpenDoor [Door] [
  [name (NounPhrase "open door")]
  [char "|"]

  [smooth True]
  [close-time (fn [self]
    (+ 5 (expovariate (/ 1 (* 60 (inc G.dungeon-level))))))]
      ; Yes, doors take longer to close at deeper levels, even
      ; though that makes things easier rather than harder. The
      ; point is to keep the question of whether a door will
      ; close within a given period of time practically
      ; uncertain. Later levels involve more travel and bigger
      ; time losses due to obstacles, so we have to compensate.

  [__init__ (fn [self open-time]
    (Door.__init__ self)
    (.schedule self)
    (.take-time self (+ open-time (.close-time self)))
    None)]

  [act (fn [self]
    (if (.at (rtv-get creature.Creature) self.pos)
      (.wait self)
      (do
        (mset self.pos (ClosedDoor))
        (.deschedule self))))]])

(defclass HasExitTime [object] [

  [min-exit-time (fn [self] 1)]
  [max-exit-time (fn [self] (inc (* 2 G.dungeon-level)))]])

(defclass Slime [Tile HasExitTime] [
  [name (kwc NounPhrase "slime" :+mass :unit "puddles")]
  [char "}"]
  [info-text "Stepping into this mystery sludge is easy enough, but stepping out of it will take some time as you free yourself."]
    ; The term "mystery sludge" is from MXC.
  [color-fg :white]
  [color-bg :dark-green]

  [unpleasant True]

  [step-out-of (fn [self cr p-to]
    (unless (or cr.flying cr.slime-immune)
      (cr.take-time (randint (.min-exit-time self) (.max-exit-time self)))))]])

(defclass Web [Tile HasExitTime] [
  [name (NounPhrase "spiderweb")]
  [char "%"]
  [color-fg :dark-blue]
  [info-text "This sizable web will make stepping out of its tile difficult, just like slime. Furthermore, while you're standing in a web, you can't apply or drop items."]

  [unpleasant True]

  [use-item-while-here (fn [self]
    (if G.player.web-immune
      True
      (do
        (msg :tara "{p:The} is stuck in the web. {p:He} can't use items.")
        False)))]

  [step-out-of (fn [self cr p-to]
    (unless cr.web-immune
      (cr.take-time (randint (.min-exit-time self) (.max-exit-time self)))))]])

(defclass Ice [Tile] [
  [name (kwc NounPhrase "patch of ice"
    :plural "patches of ice")]
  [char ":"]
  [color-fg :white]
  [color-bg :pale-azure]
  [info-text "A slippery sort of tile. If the next thing you do after stepping on it is move again in the same direction, you'll glide along without an issue. But if you take any other action, you'll need to take a moment to steady yourself first."]

  [unpleasant True]
  [smooth True]

  [min-slip-time (fn [self] 1)]
  [max-slip-time (fn [self] (inc (* 2 G.dungeon-level)))]

  [after-step-onto (fn [self cr p-from]
    (unless (or cr.flying (.ice-immune cr))
      ; Set up the creature to slip.
      (setv cr.ice-slip-towards (- self.pos p-from))
      (setv cr.ice-slip-time (randint (.min-slip-time self) (.max-slip-time self)))))]

  [step-out-of (fn [self cr p-to]
    ; If the creature moves in the same direction it moved to
    ; get here, it doesn't slip.
    (when (= (- p-to self.pos) cr.ice-slip-towards)
      (cr.reset-ice-slipping)))]])
