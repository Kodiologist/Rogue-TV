(require kodhy.macros roguetv.macros)

(import
  [heidegger.pos [Pos]]
  [kodhy.util [ret]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.map [Tile upelevator-pos room-for? disc-taxi]]
  [roguetv.item.generic [Item ItemAppearance def-itemtype]]
  [roguetv.creature [Creature Stink Haste Strength Sleep]])

(defclass Soda [Item] [
  [apply-time 1]
  [char "!"]

  [info-unidentified "A Rogue TV-branded insulated miniature aluminum can of some unidentifiable beverage. 'a'pply it to chug it and find out what it is."]

  [applied (fn [self] (block
    (kwc .identify self :+consumed)
    (G.player.take-time self.apply-time)
    (.remove G.inventory self)
    (self.soda-effect)))]])

(def appearances {
  "blank"                    :white
  "foreign-language-labeled" :dark-blue
  "polka-dot"                :orange
  "striped"                  :dark-green
  "garishly colored"         :purple
  "off-white"                :dark-gray
  "red-checked"              :red
  "houndstooth"              :black
  "reddish-greenish"         :green
  "tie-dye"                  :yellow
  "reflective"               :dark-gray
  "cornflower-blue"          :blue
  "chartreuse"               :green
  "maroon"                   :red
  "scarlet"                  :dark-red
  "hot-pink"                 :purple})
(setv (get ItemAppearance.registry Soda) (lc
  [[name color] (.items appearances)]
  (kwc ItemAppearance
    :name (NounPhrase (+ name " soda can"))
    :color-fg color)))

(defn can-of [s &optional force-a]
  (kwc NounPhrase
    :stem (+ "can of " s)
    :indefinite-singular (when force-a (+ "a can of " s))))

(def-itemtype Soda "chicken-soup" :name (can-of "chicken soup")
  :price 10
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

(def-itemtype Soda "heeling-potion" :name (kwc NounPhrase "potion of extra heeling" :plural "potions of extra heeling")
  ; A pun on Rogue's potion of extra healing.
  :price 10
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
    (msg (cond
      [(= summoned 1)
        "Hey, where'd that dog come from?"]
      [(> summoned 1)
        "Hey, where'd those dogs come from?"]
      [True
        "You hear plaintive barking."]))))

(def-itemtype Soda "stink-serum" :name (can-of "stink serum")
  ; Inspired by Yipe! III.
  :price 3
  :info-flavor "This refreshing beverage has an odd but tasty flavor with notes of beans, Limburger cheese, durian, and asparagus. The, ah, aftereffects are somewhat less pleasant."
  :stink-time 30

  :info-apply (.format "You'll stink for {{stink_time}} seconds. While you stink, monsters within {} squares will run away from you." G.stink-range)
  :soda-effect (fn [self]

    (.add-effect G.player Stink self.stink-time
      (fn [] (msg :aud "cries out in disgust at the pungent odor."))
      (fn [] (msg :tara "Smells like {p:the} is going to keep on smelling for a while.")))))

(def-itemtype Soda "speed-soda" :name (kwc can-of "5-minute ENERGY™" :+force-a)
    ; We need :+force-a because of a bug in inflect.
  ; In reference to the real dietary supplement 5-hour Energy.
  :price 15
  :info-flavor "He's got go power! He's feeling his—aw, phooey, wrong cue card. Anyway, this is some kind of swill that you don't really want to know the origin or chemical composition of, but it's got quite a kick, for a short time."
    ; Mid-20th-century Cheerios ads
  :haste-time (* 5 60)
  ; See also G.speedup-soda-factor.

  :info-apply "Doubles your walking speed for five minutes."
  :soda-effect (fn [self]

    (.add-effect G.player Haste self.haste-time
      (fn [] (msg "You feel jittery."))
      (fn [] (msg "Your jittering intensifies.")))))
        ; http://knowyourmeme.com/memes/intensifies

(def-itemtype Soda "strength-soda" :name (can-of "Daffy's Elixir")
  ; A name for several patent medicines.
  :price 3
  :info-flavor "This marvelous concoction will give you the strength of a raging bull!"
  :strength-time (* 5 60)

  :info-apply "Allows you to instantly open doors for five minutes."
  :soda-effect (fn [self]

    (.add-effect G.player Strength self.strength-time
      (fn [] (msg "You feel strong."))
      (fn [] (msg "You feel ready for more gainz.")))))
        ; Bodybuilding slang.

(def-itemtype Soda "sleep-soda" :name (can-of "Ovaltine®")
  :price 5
  :info-flavor "Here is that <b>drugless</b> way to quiet your ragged nerves so many people are asking about today. Ovaltine® marks one of the most important scientific findings of its time."
    ; Quoting from old Ovaltine ads:
    ; - Collier's, 8 October 1932, p. 29 - http://web.archive.org/http://i.imgur.com/zX4Axys.jpg
    ; - Milwaukee Sentinel, 13 Oct 1928, p. 21 - http://web.archive.org/http://i.imgur.com/Q132P5l.png
  :sleep-time 30

  :info-apply "Makes you fall asleep for {sleep_time} seconds."
  :soda-effect (fn [self]

    (.add-effect G.player Sleep self.sleep-time
      (fn [] (msg :tara "Oh no! {p:The} has fallen asleep!"))
      (fn [] (msg "You snore.")))))
