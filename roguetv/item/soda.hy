(require kodhy.macros roguetv.macros)

(import
  [heidegger.pos [Pos]]
  [kodhy.util [ret]]
  [roguetv.strings [soda-cans]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.map [Tile upelevator-pos room-for? disc-taxi]]
  [roguetv.item.generic [Item ItemAppearance def-itemtype]]
  [roguetv.creature [Creature Stink Haste Confusion Strength Sleep Passwall]])

(defclass Soda [Item] [
  [apply-time 1]
  [char "!"]

  [info-unidentified "A Rogue TV-branded insulated miniature aluminum can of some unidentifiable beverage. 'a'pply it to chug it and find out what it is."]

  [applied (fn [self] (block
    (setv was-id? (.identified? self))
    (.identify self)
    (unless was-id?
      (msg "You had:  {}" (self.invstr)))
    (unless (afind-or it.carry-instant-soda-use (active-inv))
      (G.player.take-time self.apply-time))
    (.remove G.inventory self)
    (.destroy self)
    (self.soda-effect)))]])

(defn can-of [s]
  (kwc NounPhrase
    :stem (+ "can of " s)
    :article "a"))

(def-itemtype Soda "chicken-soup" :name (can-of "chicken soup")
  :color-fg :yellow
  :level-lo 3
  :info-flavor "Tastes like the comforts of home. And it's still piping hot."
  :radius-around-upelv 5

  :info-apply "Teleports you back to the up elevator. You'll appear at a random available tile within {radius_around_upelv} squares of the up elevator."
  :soda-effect (fn [self] (block

    (for [p (shuffle (disc-taxi (upelevator-pos) self.radius-around-upelv))]
      (when (= p G.player.pos)
        (msg :bob "You've always had the power to go back to Kansas.")
          ; The Wizard of Oz
        (ret))
      (when (room-for? G.player p)
        (msg "You teleport near the up elevator." self)
        (.move G.player p)
        (ret)))
    (msg "You feel homesick."))))

(def-itemtype Soda "heeling-potion" :name "potion of extra heeling"
  ; A pun on Rogue's potion of extra healing.
  :color-fg :brown
  :price-adj :bad-flavor
  :level-lo 3
  :level-hi 7
  :info-flavor "Drink this magic potion and dogs will come heeling from far and wide."
  :dog-summoning-range 3
  :dogs-to-summon 5

  :info-apply "Creates {dogs_to_summon} dogs within {dog_summoning_range} squares of you."
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

(def-itemtype Soda "stink-serum" :name (can-of "stink serum")
  ; Inspired by Yipe! III.
  :color-fg :dark-green
  :level-lo 1
  :info-flavor "This refreshing beverage has an odd but tasty flavor with notes of beans, Limburger cheese, durian, and asparagus. The, ah, aftereffects are somewhat less pleasant."
  :stink-time 30

  :info-apply (.format "You'll stink for {{stink_time}} seconds. While you stink, monsters within {} squares will run away from you." G.repulsed-from-player-range)
  :soda-effect (fn [self]

    (.add-to-player Stink self.stink-time
      (fn [] (msg :aud "cries out in disgust at the pungent odor."))
      (fn [] (msg :tara "Smells like {p:the} is going to keep on smelling for a while.")))))

(def-itemtype Soda "speed-soda" :name (can-of "5-second ENERGY™")
  ; In reference to the real dietary supplement 5-hour Energy.
  :color-fg :red
  :level-lo 4
  :info-flavor "He's got go power! He's feeling his—aw, phooey, wrong cue card. Anyway, compared to its namesake, which is basically caffeine, this novel beverage is of mysterious origin, and it's got a veritably supernatural kick, for a (very, very) short time."
    ; Mid-20th-century Cheerios ads
  :haste-time 5

  :info-apply (.format "Increases your walking speed by a factor of {} for {{haste_time}} seconds." G.speedup-soda-factor)
  :soda-effect (fn [self]

    (.add-to-player Haste self.haste-time
      (fn [] (msg "You feel extremely jittery."))
      (fn [] (msg "Your jittering intensifies.")))))
        ; http://knowyourmeme.com/memes/intensifies

(def-itemtype Soda "confusion-soda" :name (can-of "booze")
  :color-fg :black
  :price-adj :bad-flavor
  :level-hi 5
  :info-flavor "This is a generous portion of the most popular recreational drug in history, possibly excepting caffeine. Did you know that in 2012, about 3 million deaths (6% of all deaths worldwide) were attributable to alcoholic beverages? Seriously, folks, if you must drink, be very careful about how much you drink and what you do while intoxicated. Anyway, back to your regularly scheduled dumb jokes."
    ; World Health Organization. (2014). Global status report on alcohol and health 2014. Retrieved from http://www.who.int/substance_abuse/publications/global_alcohol_report
  :confusion-time 45

  :info-apply "Confuses you for {confusion_time} seconds. While confused, you have a chance of walking or pointing in the wrong direction."
  :soda-effect (fn [self]

    (.add-to-player Confusion self.confusion-time
      (fn [] (msg "Wow, that'shh good shhtuff."))
      (fn [] (msg :tara "Keep your head in the game, {p}.")))))

(def-itemtype Soda "strength-soda" :name (can-of "Daffy's Elixir")
  ; A name for several patent medicines.
  :color-fg :dark-blue
  :level-hi 6
  :info-flavor "This marvelous concoction will give you the strength of a raging bull!"
  :strength-minutes 3

  :info-apply "Allows you to instantly open doors and chests for {strength_minutes} minutes."
  :soda-effect (fn [self]

    (.add-to-player Strength (* 60 self.strength-minutes)
      (fn [] (msg "You feel strong."))
      (fn [] (msg "You feel ready for more gainz.")))))
        ; Bodybuilding slang.

(def-itemtype Soda "passwall-soda" :name (can-of "pass-through punch")
  :color-fg :dark-red
  :level-lo 7
  :info-flavor "It lets you walk through walls! Too bad it doesn't let you see through walls."
  :passwall-time 45

  :info-apply "Allows you to walk through soild obstacles for {passwall_time} seconds. If you're inside a wall when the time runs out, you'll be ejected to the nearest free space."
  :soda-effect (fn [self]

    (.add-to-player Passwall self.passwall-time
      (fn [] (msg "You feel ethereal."))
      (fn [] (msg "You feel more subtle.")))))

(def-itemtype Soda "sleep-soda" :name (can-of "Ovaltine®")
  :color-fg :dark-orange
  :price-adj :bad-flavor
  :level-lo 6
  :info-flavor (.join "\n\n" ["Here is that <b>drugless</b> way to quiet your ragged nerves so many people are asking about today. Ovaltine marks one of the most important scientific findings of its time."
    "(No, seriously, those are exact quotes from Ovaltine ads from the 20s and 30s. So, the questionable claims ads make about dietary supplements these days are not so new. Ovaltine, at least, has cleaned up its act by limiting its claims to the observation that it's micronutrient-foritifed and that these micronutrients are essential for health.)"])
    ; - Collier's, 8 October 1932, p. 29 - http://web.archive.org/http://i.imgur.com/zX4Axys.jpg
    ; - Milwaukee Sentinel, 13 Oct 1928, p. 21 - http://web.archive.org/http://i.imgur.com/Q132P5l.png
  :sleep-time 30

  :info-apply "Makes you fall asleep for {sleep_time} seconds."
  :soda-effect (fn [self]

    (.add-to-player Sleep self.sleep-time
      (fn [] (msg :tara "Oh no! {p:The} has fallen asleep!"))
      (fn [] (msg "You snore.")))))

(setv (get ItemAppearance.registry Soda) (amap
  (ItemAppearance it (NounPhrase (+ it " soda can")))
  soda-cans))
(assert (>= (len (get ItemAppearance.registry Soda))
  (len (filt (instance? Soda it) (.values G.itypes)))))
