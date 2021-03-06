(require [kodhy.macros [amap filt afind-or block meth]] [roguetv.macros [*]])

(import
  [random [choice]]
  [heidegger.pos [Pos]]
  [kodhy.util [ret]]
  [roguetv.strings [soda-cans]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.map [Tile all-pos upelevator-pos room-for? disc-taxi]]
  [roguetv.item.generic [Item ItemAppearance def-itemtype]]
  [roguetv.creature [Creature Stink Haste Confusion Strength Passwall Hallucinating]])

(defclass Soda [Item] [
  apply-time (seconds 1)
  char "!"

  info-unidentified "A Rogue TV-branded insulated miniature aluminum can of some unidentifiable beverage. 'a'pply it to chug it and find out what it is."

  applied (fn [self] (block
    (setv was-id? (.identified? self))
    (.identify self)
    (unless was-id?
      (msg "You had:  {}" (self.invstr)))
    (unless (afind-or it.carry-instant-soda-use (active-inv))
      (.take-time G.player self.apply-time))
    (.remove G.inventory self)
    (.destroy self)
    (self.soda-effect)))])

(defclass EffectSoda [Soda] [
  effect None
  effect-time (seconds 1)
  start-msg None
  lengthen-msg None

  soda-effect (fn [self]
    (.add-to-player self.effect self.effect-time
      (fn [] (if (string? self.start-msg)
        (msg self.start-msg)
        (msg #* self.start-msg)))
      (fn [] (if (string? self.lengthen-msg)
        (msg self.lengthen-msg)
        (msg #* self.lengthen-msg)))))])

(defn can-of [s]
  (NounPhrase
    :stem (+ "can of " s)
    :article "a"))

(def-itemtype Soda "chicken-soup" :name (can-of "chicken soup")
  :color-fg :yellow
  :level-lo 3
  :info-flavor "Tastes like the comforts of home. And it's still piping hot."
  :radius-around-upelv 5

  :info-apply "Teleports you back to the up elevator. You'll appear at a random available tile within {radius-around-upelv} squares of the up elevator."
  :soda-effect (fn [self] (block

    (for [p (shuffle (disc-taxi (upelevator-pos) self.radius-around-upelv))]
      (when (= p G.player.pos)
        (msg 'bob "You've always had the power to go back to Kansas.")
          ; The Wizard of Oz
        (ret))
      (when (room-for? G.player p)
        (msg "You teleport near the up elevator." self)
        (.move G.player p)
        (ret)))
    (msg "You feel homesick."))))

(def-itemtype Soda "elsewhere-soda" :name (can-of "elsewhere eggnog")
  :color-fg :white
  :level-lo 8
  :info-flavor "It's always Christmas somewhere, right? Isn't that how time zones work?"
  :dist-min-quantile 2/3

  :info-apply "Teleports you to a faraway part of the level. Specifically, the distances of all available tiles from you are checked, and you are teleported to a tile with a distance beyond the {dist-min-quantile} quantile."
  :soda-effect (meth [] (block

    (setv candidates
      (amap (, (dist-taxi G.player.pos it) it)
      (filt (room-for? G.player it)
      (all-pos))))
    (unless candidates
      (msg "You feel cramped.")
      (ret))
    (setv candidates (cut (sorted :key first candidates) (min
      (dec (len candidates))
      (round (* (len candidates) @dist-min-quantile)))))
    (.move G.player (second (choice candidates)))
    (msg "You teleport away."))))

(def-itemtype Soda "heeling-potion" :name "potion of extra heeling"
  ; A pun on Rogue's potion of extra healing.
  :color-fg :brown
  :price-adj :bad-flavor
  :level-lo 3
  :level-hi 7
  :info-flavor "Drink this magic potion and dogs will come heeling from far and wide."
  :dog-summoning-range 3
  :dogs-to-summon 5

  :info-apply "Creates {dogs-to-summon} dogs within {dog-summoning-range} squares of you."
  :soda-effect (fn [self]

    (setv summoned 0)
    (for [p (shuffle (disc-taxi G.player.pos self.dog-summoning-range))]
      (when (room-for? Creature p)
        (rtv creature.monster.Dog p)
        (+= summoned 1)
        (when (> summoned self.dogs-to-summon)
          (break))))
    (msg (if summoned
      (.format "Hey, where'd {} come from?"
        (if (= summoned 1) "that dog" "those dogs"))
      "You hear plaintive barking."))))

(def-itemtype EffectSoda "stink-serum" :name (can-of "stink serum")
  ; Inspired by Yipe! III.
  :color-fg :dark-green
  :level-lo 1
  :info-flavor "This refreshing beverage has an odd but tasty flavor with notes of beans, Limburger cheese, durian, and asparagus. The, ah, aftereffects are somewhat less pleasant."

  :info-apply "You'll stink for {effect-time}. While you stink, monsters within {G.repulsed-from-player-range} squares will run away from you."
  :effect Stink
  :effect-time (seconds 30)
  :start-msg ['aud "cries out in disgust at the pungent odor."]
  :lengthen-msg ['tara "Smells like {p:the} is going to keep on smelling for a while."])

(def-itemtype EffectSoda "speed-soda" :name (can-of "5-second ENERGY™")
  ; In reference to the real dietary supplement 5-hour Energy.
  :color-fg :red
  :level-lo 4
  :info-flavor "He's got go power! He's feeling his—aw, phooey, wrong cue card. Anyway, compared to its namesake, which is basically caffeine, this novel beverage is of mysterious origin, and it's got a veritably supernatural kick, for a (very, very) short time."
    ; Mid-20th-century Cheerios ads

  :info-apply "Increases your walking speed by a factor of {G.haste-factor} for {effect-time}."
  :effect Haste
  :effect-time (seconds 5)
  :start-msg "You feel extremely jittery."
  :lengthen-msg "Your jittering intensifies.")
    ; http://knowyourmeme.com/memes/intensifies

(def-itemtype (get G.itypes "speed-soda") "speed-soda-2" :name (can-of "5-minute ENERGY™")
  :level-lo 14
  :info-flavor "This extra-strength new-and-improved reformulation of 5-second ENERGY™ is only mildly radioactive."

  :effect-time (minutes 5))

(def-itemtype EffectSoda "confusion-soda" :name (can-of "booze")
  :color-fg :black
  :price-adj :bad-flavor
  :level-hi 5
  :info-flavor "This is a generous portion of the most popular recreational drug in history, possibly excepting caffeine. Did you know that in 2012, about 3 million deaths (6% of all deaths worldwide) were attributable to alcoholic beverages? Seriously, folks, if you must drink, be very careful about how much you drink and what you do while intoxicated. Anyway, back to your regularly scheduled dumb jokes."
    ; World Health Organization. (2014). Global status report on alcohol and health 2014. Retrieved from http://www.who.int/substance_abuse/publications/global_alcohol_report

  :info-apply "Confuses you for {effect-time}. While confused, you have a chance of walking or pointing in the wrong direction."
  :effect Confusion
  :effect-time (seconds 45)
  :start-msg "Wow, that'shh good shhtuff."
  :lengthen-msg ['tara "Keep your head in the game, {p}."])

(def-itemtype EffectSoda "strength-soda" :name (can-of "Daffy's Elixir")
  ; A name for several patent medicines.
  :color-fg :dark-blue
  :level-hi 6
  :info-flavor "This marvelous concoction will give you the strength of a raging bull!"

  :info-apply "Allows you to instantly open doors and chests for {effect-time}."
  :effect Strength
  :effect-time (minutes 3)
  :start-msg "You feel strong."
  :lengthen-msg "You feel ready for more gainz.")
    ; Bodybuilding slang.

(def-itemtype EffectSoda "passwall-soda" :name (can-of "pass-through punch")
  :color-fg :dark-red
  :level-lo 7
  :info-flavor "It lets you walk through walls! Too bad it doesn't let you see through walls."

  :info-apply "Allows you to walk through solid obstacles for {effect-time}. If you're inside a wall when the time runs out, you'll be ejected to the nearest free space."
  :effect Passwall
  :effect-time (seconds 45)
  :start-msg "You feel ethereal."
  :lengthen-msg "You feel more subtle.")

(def-itemtype EffectSoda "hallu-soda" :name (can-of "Mountain Dew®")
  :color-fg :dark-green
  :rarity :uncommon
  :price-adj :bad-flavor
  :info-flavor "DO THE DEW®"

  :info-apply "Makes you MLG for {effect-time}. What does MLG stand for, anyway?"
  :effect Hallucinating
  :effect-time (minutes 3)
  :start-msg "You feel ready to rek some scrublords."
  :lengthen-msg ['bob "Wombo combo!"])

(def-itemtype Soda "effect-extend-soda" :name (can-of "effect-extending elixir")
  :color-fg :dark-red
  :level-lo 11
  :info-flavor "Get the extra mile out of your magic potions."

  :info-apply "Doubles the durations of all active status effects."
  :soda-effect (meth [] (block
 
    (unless G.player.effects
      (msg 'bob "Oooh, that was a waste.")
      (ret))

    (for [effect G.player.effects]
      (.take-time effect (- effect.next-turn G.current-time)))
    (msg "You feel more special."))))

(def-itemtype Soda "effect-end-soda" :name (can-of "effect-ending elixir")
  :color-fg :dark-red
  :level-lo 9
  :info-flavor "Cures what ails ya, but also what benefits ya, so watch out for that."

  :info-apply "Ends all active status effects."
  :soda-effect (meth [] (block
 
    (unless G.player.effects
      (msg "You have a normal feeling for a moment; then it passes.")
      (ret))

    (for [effect G.player.effects]
      (.destroy effect)))))

(def-itemtype Soda "sleep-soda" :name (can-of "Ovaltine®")
  :color-fg :dark-orange
  :price-adj :bad-flavor
  :level-lo 6
  :info-flavor (.join "\n\n" ["Here is that <b>drugless</b> way to quiet your ragged nerves so many people are asking about today. Ovaltine marks one of the most important scientific findings of its time."
    "(No, seriously, those are exact quotes from Ovaltine ads from the 20s and 30s. So, the questionable claims ads make about dietary supplements these days are not so new. Ovaltine, at least, has cleaned up its act by limiting its claims to the observation that it's micronutrient-foritifed and that these micronutrients are essential for health.)"])
    ; - Collier's, 8 October 1932, p. 29 - http://web.archive.org/http://i.imgur.com/zX4Axys.jpg
    ; - Milwaukee Sentinel, 13 Oct 1928, p. 21 - http://web.archive.org/http://i.imgur.com/Q132P5l.png
  :sleep-time (seconds 30)

  :info-apply "Makes you fall asleep for {sleep-time}."
  :soda-effect (fn [self]

    (.fall-asleep G.player self.sleep-time)
    (msg 'tara "Oh no! {p:The} has fallen asleep!")))

(setv (get ItemAppearance.registry Soda) (amap
  (ItemAppearance it (NounPhrase (+ it " soda can")))
  soda-cans))
(assert (>= (len (get ItemAppearance.registry Soda))
  (len (filt (instance? Soda it) (.values G.itypes)))))
