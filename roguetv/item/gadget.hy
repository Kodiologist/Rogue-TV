(require [kodhy.macros [amap filt afind-or whenn block retf meth]] [roguetv.macros [*]])

(import
  [math [*]]
  [random [randrange choice]]
  [heidegger.pos [Pos]]
  [kodhy.util [ret]]
  [roguetv.strings [gadget-adjectives]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [input-direction inventory-loop]]
  [roguetv.map [Tile Floor Wall Door Chest Ice Web on-map room-for? mset mget ray-taxi disc-taxi]]
  [roguetv.item.generic [Item ItemAppearance def-itemtype get-other-item]]
  [roguetv.creature [Creature]])

(defclass Gadget [Item] [
  max-charges None
  apply-time (seconds 1)
  char "/"

  info-unidentified "This is some bizarre gizmo from a late-night infomercial included in Rogue TV as product placement. Goodness knows what it does; it could as easily be a soldering iron as a waffle iron. 'a'pply it to use it and find out. Each use will consume one of a limited number of charges. If the gadget can be aimed or otherwise controlled, you'll only be able to control it once you know what it is."

  __init__ (fn [self &kwargs kw]
    (apply Item.__init__ [self] kw)
    (setv self.charges (.get kw "charges" self.max-charges))
    None)

  clone-setup (fn [self orig]
    (.clone-setup (super Gadget self) orig)
    (setv self.charges orig.charges))

  name-suffix (fn [self]
    (when (.identified? self)
      (.format "({}/{})" self.charges self.max-charges)))

  applied (fn [self] (block
    ; Do we have a charge to spare?
    (when (= self.charges 0)
      (msg 'bob "That oojah's all chatty, kemosabe.")
      (ret))
    ; Do we succeed in applying the gadget?
    (for [x (filt it.carry-gadget-malfunction-1in (active-inv))]
      (when (1-in x.carry-gadget-malfunction-1in)
        (.use-time-and-charge self)
        (msg "Oops, wrong button. There goes a charge.")
        (ret)))
    ; Identify the item type.
    (setv unid (not (.identified? self)))
    (.identify self)
    (when unid
      (msg "You have:  {}" (self.invstr)))
    ; Now you get the gadget effect.
    (self.gadget-effect unid)))

  gadget-effect (fn [self unid]
    ; Do whatever the gadget should do. `unid` says whether
    ; the item was unidentified before this use.
    ;
    ; If the user ends up really getting the gadget effect (they
    ; don't, e.g., cancel out of a direction prompt), be sure to
    ; call .use-time-and-charge before otherwise affecting the
    ; game world.
    (.use-time-and-charge self)
    (msg "Nothing happens."))

  use-time-and-charge (fn [self]
    ; Use up a charge and some time.
    (unless (afind-or it.carry-instant-gadget-use (active-inv))
      (.take-time G.player self.apply-time))
    (-= self.charges 1))])

(def-itemtype Gadget "panic-button" :name "panic button"
  :color-fg :red
  :level-lo 5
  :info-flavor "Press it if you expect to be particularly lucky in the future, or if you are particularly unlucky in the present."
  :max-charges 3
  :teleport-tries 100

  :info-apply "Teleports you to a random square elsewhere on the current dungeon level."
  :gadget-effect (fn [self unid] (block :gadget

    (.use-time-and-charge self)

    ; Find a place to teleport to.
    (block
      (for [_ (range self.teleport-tries)]
        (setv p-to (Pos (randrange G.map-width) (randrange G.map-height)))
        (when (and
            (!= p-to G.player.pos)
            (room-for? G.player p-to)
            (instance? Floor (Tile.at p-to)))
          (ret)))
      ; We failed to find a legal square.
      (msg "You feel cramped.")
      (retf :gadget))

    ; Now teleport there.
    (.move G.player p-to)
    (msg 'tara "{p:He's} teleported to another part of the level."))))

(def-itemtype Gadget "warpback" :name "warpback machine"
  :color-fg :dark-orange
  :level-lo 6
  :rarity :uncommon
  ; Has an extra instance attribute .warpback-pos.
  :info-flavor "It's deja vu all over again."
  :max-charges 3

  :__init__ (fn [self &kwargs kw]
    (apply (. (super (get G.itypes "warpback") self) __init__) [] kw)
    (setv self.warpback-pos None))

  ; No clone-setup is provided because we don't want warpback-pos
  ; to be copied, anyway.

  :on-reset-level (fn [self]
    (setv self.warpback-pos None))

  :info-apply "Use it once to register a warpback point (for you, sir, no charge). Use it again to teleport back to the warpback point. This will clear the warpback point, as will going to another dungeon level."
  :gadget-effect (fn [self unid]

    (if (none? self.warpback-pos)
      (do
        (setv self.warpback-pos G.player.pos)
        (msg "{:The} registers your current position." self))
      (do
        (.use-time-and-charge self)
        (cond
          [(= G.player.pos self.warpback-pos) (do
            (msg "You momentarily flicker out of existence.")
            (setv self.warpback-pos None))]
          [(room-for? G.player self.warpback-pos) (do
            (msg "You reappear at {:the}'s registered location." self)
            (.move G.player self.warpback-pos)
            (setv self.warpback-pos None))]
          [True
            (msg "{:The} beeps at you accusingly." self)])))))

(def-itemtype Gadget "hookshot"
  :color-fg :dark-blue
  :level-lo 4
  :info-flavor "Arfer's law of game design: any video game is improved by the addition of a grappling hook."
  :max-charges 10
  :hookshot-dist 30
  :hookshot-travel-speed 2

  :info-apply "Fire it at a solid obstacle up to {hookshot-dist} squares away to get yourself over there. Travel by hookshot is {hookshot-travel-speed} times as fast as travel by foot, and you'll pass over unpleasant terrain along the way. Creatures will block the hookshot."
  :gadget-effect (fn [self unid] (block :gadget

    (setv d (if unid (choice Pos.DIR8) (or (input-direction) (ret))))
    (.use-time-and-charge self)

    ; Find our destination square.
    (setv ahead (+ G.player.pos d))
    (setv path (ray-taxi ahead d self.hookshot-dist
      :include-off-map True))
    (block
      (for [p path]
        (unless (on-map p)
          (ret))
        (when (. (Tile.at p) blocks-movement)
          (ret))
        (whenn (Creature.at p)
          (msg 'tara "{p:The}'s {} bounces off {:the}."
            self it)
          (retf :gadget)))
      (msg "{:Your} doesn't hit anything. It can only reach objects up to {} squares away."
        self self.hookshot-dist)
      (retf :gadget))
    (setv p-to (- p d))
    (when (= p-to G.player.pos)
      (msg "{:Your} uselessly hits {:the} right next to you."
        self (mget ahead))
      (retf :gadget))

    ; And away we go.
    (.take-time G.player (seconds (/
      (len-taxi (- p-to G.player.pos))
      self.hookshot-travel-speed)))
    (.move G.player p-to)
    (msg "{:The} pulls you ahead." self))))

(def-itemtype (get G.itypes "hookshot") "longshot"
  :color-fg :dark-blue
  :level-lo 15
  :info-flavor "Corollary to Arfer's law of game design: a video game that already has a grappling hook is further improved by the addition of another grappling hook."
  :max-charges 10
  :hookshot-dist 100
  :hookshot-travel-speed 4)

(def-itemtype Gadget "switch-hook" :name (NounPhrase "switch hook")
  :color-fg :gold
  :level-lo 9
  :info-flavor "The switch hook is the oddball of the grappling-hook family. It has a mysterious power of exchange."
  :max-charges 10
  :hook-dist 15

  :info-apply "Fire it at another creature up to {hook-dist} squares away to instantly change places, teleporting you to its position and it to yours."
  :gadget-effect (meth [unid] (block :gadget

    (setv d (if unid (choice Pos.DIR8) (or (input-direction) (ret))))
    (@use-time-and-charge)

    ; Find the monster we're switching places with.
    (setv ahead (+ G.player.pos d))
    (setv path (ray-taxi ahead d @hook-dist :include-off-map True))
    (block
      (for [p path]
        (when (and (on-map p) (Creature.at p))
          (ret))
        (when (. (mget p) blocks-movement)
          (msg 'tara "{p:The}'s {} bounces off {:the}."
            @@ (mget p))
          (retf :gadget)))
      (msg "{:Your} doesn't hit anything. It can only reach monsters up to {} squares away."
        @@ @hook-dist)
      (retf :gadget))
    (setv p-to p)

    ; Now switch places.
    (setv cr (Creature.at p-to))
    (.move cr G.player.pos :clobber True)
    (.move G.player p-to)
    (msg "You switch places with {:the}." cr))))

(def-itemtype Gadget "chainsaw"
  :color-fg :yellow
  :level-hi 7
  :info-flavor "Just what you need to kickstart a lucrative career in lumberjacking after winning gobs of dosh on Rogue TV. Or you could sell it for an additional gob of dosh. Whatever; I don't judge."
  :max-charges 8
  :destroy-item-1in 3

  :info-apply "Instantly destroys an adjacent door or chest. But if a chest contains an item, the item has a 1 in {destroy-item-1in} chance of also being destroyed."
  :gadget-effect (fn [self unid] (block

    (setv d (if unid (choice Pos.DIR8) (or (input-direction) (ret))))
    (setv p (+ G.player.pos d))
    (setv t (mget p))

    (.use-time-and-charge self)
    (cond
      [(instance? Door t) (do
        (msg "Bzzt! The door is no more.")
        (mset p (Floor)))]
      [(instance? Chest t) (do
        (msg "Bzzt! You put the chest to rest.")
        (whenn (.open-chest t G.player) (when (1-in self.destroy-item-1in)
          (if (.delete it)
            (msg "Oops! {:Your} destroyed {:him}." self it)
            (msg "{:He} miraculously {:v:avoids} being destroyed by {:the}." it it self)))))]
      [(and (on-map p) (Creature.at p))
        (msg 'bob "This is Rogue TV, not DoomRL!")]
      [True
        (msg "{:Your} proves ineffective against {:the}." self t)]))))

(def-itemtype Gadget "hairdryer" :name "hair dryer"
  :color-fg :black
  :level-lo 1
  :level-hi 6
  :info-flavor "Just because you're running around in a dungeon doesn't mean you can't have salon-quality hair."
  :max-charges 8
  :melt-range 10

  :info-apply "Melts all ice within {melt-range} squares."
  :gadget-effect (fn [self unid] (block

    (.use-time-and-charge self)
    (for [p (disc-taxi G.player.pos self.melt-range)]
      (when (instance? Ice (Tile.at p))
        (mset p (Floor))))
    (msg "You are briefly immersed in a cloud of warm air."))))

(def-itemtype Gadget "tunnel-machine" :name "tunnel-boring machine"
  :color-fg :dark-red
  :level-lo 2
  :info-flavor "A wicked giant drill to pound through whatever solid obstacles happen to get in your way."
  :max-charges 5
  :drill-time (seconds 3)

  :info-apply "Destroys an adjacent wall or door. This takes {drill-time}."
  :gadget-effect (fn [self unid] (block

    (setv d (if unid (choice Pos.DIR8) (or (input-direction) (ret))))
    (setv p (+ G.player.pos d))
    (setv t (mget p))

    (.use-time-and-charge self)
    (cond
      [(or (instance? Wall t) (instance? Door t)) (do
        (.take-time G.player (- self.drill-time self.apply-time))
        (msg "Your drill reduces {:the} to dust." t)
        (mset p (Floor)))]
      [(and (on-map p) (Creature.at p))
        (msg 'aud "gasps. You can't use a drill like that on a family show.")]
      [True
        (msg "Your drill proves ineffective against {:the}." t)]))))

(def-itemtype Gadget "web-machine" :name "Silly-O-Matic®"
  :color-fg :dark-blue
  :price-adj :bad-flavor
  :level-lo 4
  :info-flavor "Tired of buying can after can of generic-brand areosol string and still running out? This cutting-edge device produces aerosol string (new &amp; improved formula, patent pending; compare with SILLY STRING Brand Spray Streamer) instantly, using chemicals already present in the air of a typical American household! Just press the button and fire away! Product is flammable. Keep mouth and eyes away from exhaust port. Replace filter regularly. Do not use if you are pregnant or nursing. Check for NWS Air Quality Alerts before and after each use."
  :max-charges 10
  :web-machine-range 8

  :info-apply "Creates webs in a line up to {web-machine-range} squares long."
  :gadget-effect (fn [self unid] (block

    (setv d (if unid (choice Pos.DIR8) (or (input-direction) (ret))))
    (.use-time-and-charge self)

    (setv ahead (+ G.player.pos d))
    (setv made-a-web False)
    (for [p (ray-taxi ahead d self.web-machine-range)]
      (when (. (Tile.at p) blocks-movement)
        (break))
      (when (instance? Floor (Tile.at p))
        (mset p (Web))
        (setv made-a-web True)))

    (msg "You spray some aerosol string.")
    (unless made-a-web
      (msg 'tara "{p:The}'s spraying hasn't come to much.")))))

(def-itemtype Gadget "bee-machine" :name "personal beekeeping device"
  :color-fg :yellow
  :price-adj :bad-flavor
  :level-hi 4
  :info-flavor "A beekeeper is you! It's stuffed with everything you need to make your own honey. Including the bees. <b>Especially</b> the bees."
    ; "A foobar is you" is originally from the NES game Pro
    ; Wrestling, but is used here in a way paying tribute to
    ; Kingdom of Loathing.
  :max-charges 5
  :bee-summoning-range 3
  :bees-to-summon 10

  :info-apply "Creates {bees-to-summon} bees within {bee-summoning-range} squares of you."
  :gadget-effect (fn [self unid]

    (.use-time-and-charge self)
    (setv summoned 0)
    (for [p (shuffle (disc-taxi G.player.pos self.bee-summoning-range))]
      (when (room-for? Creature p)
        (rtv creature.monster.Bee p)
        (+= summoned 1)
        (when (> summoned self.bees-to-summon)
          (break))))
    (msg "A swarm of bees pours out of the device!")
    (cond
      [(= summoned 0)
        (msg "Then they all go right back in.")]
      [(< summoned (// self.bees-to-summon 2))
        (msg "Most of them go back in.")])))

(def-itemtype Gadget "party-cannon" :name "party cannon"
  ; http://mlp.wikia.com/wiki/Pinkie_Pie#Party_cannon
  :color-fg :hot-pink
  :level-hi 9
  :info-flavor "The one and only party-in-a-cannon. Never leave home without it."
  :max-charges 5
  :confetti-summoning-range 2
  :confetti-to-summon 5

  :info-apply "Creates {confetti-to-summon} piles of confetti within {confetti-summoning-range} squares of you."
  :gadget-effect (meth [unid] (block

    (@use-time-and-charge)
    (setv summoned 0)
    (for [p (shuffle (disc-taxi G.player.pos @confetti-summoning-range))]
      (when (room-for? Item p)
        ((get G.itypes "confetti") :pos p)
        (+= summoned 1)
        (when (> summoned @confetti-to-summon)
          (break))))
    (msg "A torrent of confetti pours out of {:the}." @@))))

(def-itemtype Item "confetti" :name (NounPhrase "confetti" :mass True :unit "piles")
  :char "*"
  :color-fg :hot-pink
  :rarity :nongen
  :price 0
  :info-flavor "A pile of little pieces of brightly colored paper. Its only function seems to be making a mess.")

(def-itemtype Gadget "microscope"
  :color-fg :dark-green
  :level-lo 7
  :rarity :uncommon
  :info-flavor "Second only to beakers full of glowing green goo in proving one's credentials as a scientist."
  :max-charges 3

  :info-apply "Identifies an item."
  :gadget-effect (fn [self unid] (block

    (setv item (or (get-other-item self unid "identify") (ret)))
    (.use-time-and-charge self)
    (msg "You inspect {:the} under {:the}." item self)
    (when (.identified? item)
      (msg "Yep, {:he's} {:a}." item item)
        ; http://knowyourmeme.com/memes/identifying-wood
      (ret))
    (.identify item)
    (msg "You have:  {}" (item.invstr)))))

(def-itemtype Gadget "food-proc" :name "food processor"
  :color-fg :brown
  :price-adj :bad-flavor
  :level-lo 3
  :info-flavor "It slices! It dices! This advanced model runs on batteries and can julienne just about anything."
    ; The Popeil Veg-O-Matic.
  :max-charges 3

  :info-apply "Permanently destroys an item."
  :gadget-effect (fn [self unid] (block

    (setv item (or (get-other-item self unid "pulverize") (ret)))

    (.use-time-and-charge self)
    (if (.delete item)
      (msg "{:Your} {:v:is} sliced and diced to bits." item item)
      (msg "Miraculously, {:the} {:v:jams} on {:your}, leaving {:him} unharmed." self self item item)))))

(def-itemtype Gadget "gps" :name "GPS device"
  :color-fg :dark-green
  :level-lo 3
  :info-flavor "They say it's unwise to use a GPS device as your only means of navigation in an unfamiliar area, but it's not as if you have lots of better options in a dungeon."
  :max-charges 5
  :gps-range 20

  :info-apply "Reveals the map in a radius of {gps-range} squares around you."
  :gadget-effect (fn [self unid]

    (.use-time-and-charge self)
    (setv did-something False)
    (for [p (disc-taxi G.player.pos self.gps-range)]
      (unless (seen p)
        (setv did-something True)
        (setv (get G.seen-map p.x p.y) True)))
    (when did-something
      (soil-fov))
    (if did-something
      (msg "{:The} reveals part of the dungeon around you." self)
      (msg "{:The} reminds you of what you already know." self))))

(setv (get ItemAppearance.registry Gadget) (amap
  (ItemAppearance it (NounPhrase (+ it " gadget")))
  gadget-adjectives))
(assert (>= (len (get ItemAppearance.registry Gadget))
  (len (filt (instance? Gadget it) (.values G.itypes)))))

(defn recharge-gadget [charger charge-factor charge-time charge-msg] (block
; Returns a boolean indicating whether the attempt succeeded.

  (setv gadget (or (get-other-item charger False "charge") (ret False)))
  (unless (instance? Gadget gadget)
    (msg "{:The} {:v:isn't} a gadget." gadget gadget)
    (ret False))

  (.take-time G.player charge-time)
  (charge-msg gadget)
  (setv gadget.charges (min gadget.max-charges
    (+ gadget.charges (int (ceil
      (* gadget.max-charges charge-factor))))))
  True))

(defclass Battery [Item] [
  char "="
  apply-time (seconds 1)
  charge-factor None

  info-flavor "This generic battery can restore charges to any gadget. 'a'pply it to charge up a gadget."

  applied (fn [self]
    (when (recharge-gadget self self.charge-factor self.apply-time
        (fn [gadget] (msg "You insert {:the} into {:the}." self gadget)))
      (.remove G.inventory self)))])

(def-itemtype Battery "battery-small" :name "button battery"
  :color-fg :red
  :price 1
  :info-apply "Restores half of a gadget's maximum charges, rounded up."
  :charge-factor .5)

(def-itemtype Battery "battery-big" :name "big battery"
  :color-fg :blue
  :price 1
  :level-lo 4
  :info-apply "Completely restores a gadget's charges."
  :charge-factor 1)
