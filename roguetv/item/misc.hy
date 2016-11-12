(require kodhy.macros)

(import
  [kodhy.util [ret]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.item.generic [Item def-itemtype get-other-item]]
  [roguetv.creature [Haste]]
  [roguetv.item.gadget [recharge-gadget]])

(def-itemtype Item "aoy"
  :name (kwc NounPhrase "Amulet of Yendor" :+the-proper)
  :char "☥"
  :color-bg :yellow
  :info-flavor "Actually, it's only a cheap plastic imitation of the Amulet of Yendor. In other words, it's a prop. The real Amulet has been lost to the ages. But this prop is worth a <b>fabulous</b> cash prize!"
  :info-carry "Allows you to take the final down elevator. In fact, the rules of Rogue TV stipulate that you're only granted the Amulet's <b>fabulous</b> cash prize if you take that down elevator. So, try not to run out of time."
  :price-grade (+ G.max-dungeon-level 10)
  :unique True
  :rarity :nongen
  :indestructible True)

(def-itemtype Item "stormbringer"
  :name (kwc NounPhrase "Stormbringer" :+bare-proper)
  :char ")"
  :color-fg :black
  :level-lo 14
  :unique True
  :rarity :rare
  :info-flavor "An ancient, malevolent demon in the form of a sword. It's about as powerful and liable to destroy what you love as it sounds."
  :apply-time (seconds 1)
  :hunger-time (minutes 5)
  :price-to-speed-time-ratio 10

  :__init__ (meth [&kwargs kw]
    (apply Item.__init__ [@] kw)
    (@schedule)
    None)

  :info-apply "Consumes an item for a temporary speed boost. Cursed items aren't eligible. The speed boost increases your walking speed by a factor of {G.haste-factor} and lasts for {apply-time} per ${price-to-speed-time-ratio} of the item's price, rounded down."
  :applied (meth []
    (whenn (get-other-item @ False "consume")
      (.take-time G.player @apply-time)
      (@consume-item it)))

  :info-carry "Occasionally activates spontaneously on a random eligible item. On average, this happens once every {hunger-time}."
  :act (meth []
    (when (in @ G.inventory)
      (setv l (filt (and (is-not it @) (not it.curse) (not it.indestructible))
        G.inventory))
      (if l
        (do
          (setv item (random.choice l))
          (msg "{:Your} {:v:hungers} for {:your}." @ @ item)
          (@consume-item item))
        (msg "{:Your} {:v:quivers} for a moment." @ @)))
    (@take-time (int (randexp @hunger-time))))

  :consume-item (meth [item] (block
    (setv price item.price)
    (when item.curse
      (msg "{:The} {:v:senses} kindred magic in {:the} and {:v:relents}." @ @ item @)
      (ret))
    (unless (.delete item)
      (msg "{:The} {:v:thirsts} for {:the}, but {:he} {:v:is} unaffected." @ @ item item item)
      (ret))
    (msg "{:The} {:v:disappears} in a burst of black flame." item item)
    (unless (>= price @price-to-speed-time-ratio)
      (msg "{:The} {:v:seems} unsatisfied." @ @)
      (ret))
    (.add-to-player Haste (seconds (inc (// price @price-to-speed-time-ratio)))
      ; The effect time is incremented so the player doesn't use it
      ; all up just waiting till their next turn.
      (fn [] (msg "Dark magic courses through your veins."))
      (fn [] (msg "Dark magic fortifies your speed."))))))

(def-itemtype Item "cyec"
  :name (kwc NounPhrase "CYEC" :+the-proper)
  :char "("
  :level-lo 10
  :unique True
  :rarity :rare
  :info-flavor "This is the celebrated Centurion Yendorian Express Card, the charge card of the rich and famous. \"Hello, this is me, ELOISE, and would you kindly send one roast-beef bone, one raisin, and seven spoons to the top floor and charge it please? Thank you very much.\""
  :apply-time (seconds 1)
  :ready-time (minutes 5)

  :__init__ (meth [&kwargs kw]
    (apply Item.__init__ [@] kw)
    (setv @ready True)
    None)

  :name-suffix (meth [] (.format "({})" (if @ready
    "ready now"
    (+ "ready in " (minsec (max 0 (- @next-turn G.current-time)))))))

  :info-apply "Restores half of a gadget's maximum charges, rounded up. The CYEC can't be applied again for {ready-time}."
  :applied (meth []
     (if @ready
        (when (recharge-gadget @ .5 @apply-time
            (fn [gadget] (msg "Cha-ching! {:The} {:v:is} charged." gadget gadget)))
          (setv @ready False)
          (@schedule)
          (@take-time @ready-time))
        (msg "You feel that {:the} {:v:is} ignoring you." @ @)))

  :act (meth []
    (@deschedule)
    (setv @ready True)
    (when (in @ G.inventory)
      (msg :tara "{p}, {:the} is ready for use again." @))))

(def-itemtype Item "test-item"
  :name "test item" :name-suffix (fn [self] "(testy)")
  :char "&"
  :info-flavor "This is a test item. It doesn't do anything."
  :price 11
  :rarity :nongen)
