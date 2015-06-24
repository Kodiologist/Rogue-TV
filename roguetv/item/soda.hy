(require kodhy.macros)

(import
  [heidegger.pos [Pos]]
  [kodhy.util [ret]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.map [Tile upelevator-pos room-for? disc-taxi]]
  [roguetv.item.generic [Item ItemAppearance def-itemtype]])

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

(defn can-of [s]
  (kwc NounPhrase
    :stem (+ "can of " s)
    :plural (+ "cans of " s)))

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

(def-itemtype Soda "stink-serum" :name (can-of "stink serum")
  ; Inspired by Yipe! III.
  :price 3
  :info-flavor "This refreshing beverage has an odd but tasty flavor with notes of beans, Limburger cheese, durian, and asparagus. The, ah, aftereffects are somewhat less pleasant."
  :stink-time 30

  :info-apply (.format "You'll stink for {{stink_time}} seconds. While you stink, monsters within {} squares will run away from you." G.stink-range)
  :soda-effect (fn [self]

    (if (>= G.current-time G.player.stink-until)
      (msg :aud "cries out in disgust at the pungent odor.")
      (msg :tara "Smells like {p:the} is going to keep on smelling for a while."))
    (setv G.player.stink-until (max G.player.stink-until
      (+ G.current-time self.stink-time)))))