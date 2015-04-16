(require kodhy.macros)

(import
  [heidegger.pos [Pos]]
  [kodhy.util [ret]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [inventory-loop]]
  [roguetv.map [mget room-for? recompute-fov]]
  [roguetv.item [Item add-to-inventory]]
  [roguetv.creature [Creature]]
  [roguetv.display [draw-inventory describe-tile]])

(defn do-normal-command [inp]

  (setv G.last-new-message-number (dec (len G.message-log)))

  (setv [cmd args] [(first inp) (slice inp 1)])
  (block (cond

    [(= cmd :quit-game)
      :quit-game]

    [(= cmd :move)
      (let [[p-from G.player.pos] [p-to (+ p-from (first args))]]
        (unless (room-for? Creature p-to)
          (ret))
        (.move G.player p-to)
        (recompute-fov)
        (describe-tile G.player.pos)
        (G.player.take-time (len-taxicab (first args))))]

    [(= cmd :examine-ground) (do
      (kwc describe-tile G.player.pos :+verbose))]

    [(= cmd :use-tile)
      (.use-tile (mget G.player.pos))]

    [(= cmd :inventory) (do
      (if G.inventory
        (kwc inventory-loop :!select "You are carrying:")
        (msgn "Your inventory is empty.")))]

    [(= cmd :pick-up) (do
      (setv item (Item.at G.player.pos))
      (when (nil? item)
        (msgn "There's nothing here to pick up.")
        (ret))
      (when (= (len G.inventory) G.inventory-limit)
        (msg :tara "{p:name} has {p:his} eyes on another prize, but {p:his} inventory is full. {p:He} can only carry up to {} items."
          G.inventory-limit)
        (ret))
      (add-to-inventory item)
      (msgn "Taken:  {}" (item.invstr))
      (G.player.take-time 1))]

    [(= cmd :drop) (do
      (unless G.inventory
        (msgn "You don't have anything to drop.")
        (ret 0))
      (setv i (inventory-loop "What do you want to drop?"))
      (when (none? i)
        ; Action canceled.
        (ret))
      (setv clear-spot (afind-or (room-for? Item it) (+
        ; Try to drop at the player's feet…
        [G.player.pos]
        ; …or at a random orthogonal neigbor…
        (shuffle (amap (+ G.player.pos it) Pos.ORTHS))
        ; …or at a random diagonal neighbor.
        (shuffle (amap (+ G.player.pos it) Pos.DIAGS)))))
      (unless clear-spot
        (msg :bob "There ain't room on the ground for that truck.")
        (ret))
      (setv item (.pop G.inventory i))
      (.move item clear-spot)
      (msgn "Dropped:  {}" (item.invstr))
      (G.player.take-time 1))]

    [True
      (raise (ValueError (.format "Unknown command {}" cmd)))])))
