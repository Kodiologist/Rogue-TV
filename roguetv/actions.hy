(require kodhy.macros roguetv.macros)

(import
  [heidegger.pos [Pos]]
  [kodhy.util [ret retf]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [text-screen message-log-screen inventory-loop look-mode y-or-n normal-command-keys]]
  [roguetv.map [Tile Wall mset]]
  [roguetv.item [Item add-to-inventory drop-pos]]
  [roguetv.creature [Creature]]
  [roguetv.display [draw-inventory describe-tile]])

(defmacro when-debugging [&rest body]
  `(if G.debug
    (do ~@body)
    (msg "That command requires debug mode.")))

(defn do-normal-command [inp]

  (setv G.last-new-message-number (dec (len G.message-log)))

  (setv cmd (if (coll? inp) (first inp) inp))
  (setv arg (when (coll? inp) (second inp)))

  (block (cond

    [(= cmd :show-controls)
      (text-screen (+
        "<b>Controls</b>\n\n"
        "Use the arrows keys, the number pad, or the vi keys to move. Use \".\" or numpad \"5\" to wait 1 second.\n\n"
        (.join "\n"
          (amap (.format "  <b>{}</b>  {}" (first it) (get it 2))
          (filt (!= (get it 2) :debug)
          normal-command-keys)))))]

    [(= cmd :resign-game)
      (if G.debug
         (retf :curses-wrapper :fast-quit)
         (when (y-or-n "Resign this game?" :+require-uppercase)
           (setv G.endgame :resigned)))]

    [(= cmd :save-and-quit)
      (retf :curses-wrapper :save-and-quit)]

    [(= cmd :move)
      (.walk-to G.player (+ G.player.pos arg))]

    [(= cmd :wait)
      (.wait G.player)]

    [(= cmd :message-log)
      (message-log-screen)]

    [(= cmd :look-mode)
      (look-mode G.player.pos)]

    [(= cmd :examine-ground) (do
      (kwc describe-tile G.player.pos :+verbose))]

    [(= cmd :use-tile)
      (.use-tile (Tile.at G.player.pos) G.player)]

    [(= cmd :inventory) (block
      (unless G.inventory
        (msg "Your inventory is empty.")
        (ret))
      (setv i (inventory-loop "You are carrying: (press a key to examine)"))
      (when (none? i)
        ; No item chosen to examine.
        (ret))
      (text-screen (.information (get G.inventory i))))]

    [(= cmd :pick-up) (do
      (setv item (Item.at G.player.pos))
      (when (nil? item)
        (msg "There's nothing here to pick up.")
        (ret))
      (when (= (len G.inventory) G.inventory-limit)
        (msg :tara "{p:The} has {p:his} eyes on another prize, but {p:his} inventory is full. {p:He} can only carry up to {} items."
          G.inventory-limit)
        (ret))
      (G.player.take-time 1)
      (add-to-inventory item)
      (msg "Taken:  {}" (item.invstr)))]

    [(= cmd :drop) (do
      (unless (.use-item-while-here (Tile.at G.player.pos))
        (ret))
      (unless G.inventory
        (msg "You don't have anything to drop.")
        (ret))
      (setv i (inventory-loop "What do you want to drop?"))
      (when (none? i)
        ; Action canceled.
        (ret))
      (setv item (get G.inventory i))
      (when item.curse
        (msg :tara "{:The} {:v:is} cursed! {p:The} can't drop {:him}."
          item item item)
        (ret))
      (setv clear-spot (drop-pos G.player.pos))
      (unless clear-spot
        (msg :bob "There ain't room on the ground for that truck.")
        (ret))
      (G.player.take-time 1)
      (.pop G.inventory i)
      (.move item clear-spot)
      (msg "Dropped:  {}" (item.invstr)))]

    [(= cmd :apply-item) (do
      (unless (.use-item-while-here (Tile.at G.player.pos))
        (ret))
      (unless G.inventory
        (msg "You aren't carrying any items to apply.")
        (ret))
      (setv i (inventory-loop "What do you want to apply?"))
      (when (none? i)
        ; Action canceled.
        (ret))
      (setv item (get G.inventory i))
      (.applied item))]

    [(= cmd :make-wall) (when-debugging
      (mset G.player.pos (Wall)))]

    [(= cmd :reset-level) (when-debugging
      (rtv mapgen.reset-level))]

    [True
      (raise (ValueError (.format "Unknown command {!r}" cmd)))])))
