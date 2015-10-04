(require kodhy.macros roguetv.macros)

(import
  [kodhy.util [ret cat]]
  [roguetv.strings [gift-box-labels]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.item.generic [Item ItemAppearance def-itemtype]])

(defcls Clothing [Item]
  char "["

  open-present-time 1
  curse-on-unbox False

  info-unidentified "This festively wrapped gift box contains an item of clothing. Your only clue as to what's inside is a cryptic product code. Clothing has a special effect on you so long you carry it, but the effect is suppressed while it's in a box. 'a'pply the box to open it."

  name-suffix (meth []
    (when (and (@identified?) @boxed)
      "(boxed)"))

  info-extra (meth [] (kwc cat :sep "\n\n"
    (when @boxed
      "<b>This item is boxed</b>, suppressing its normal effect when carried. 'a'pply it to open and discard the box.")
    (when @curse-on-unbox
      "<b>This item becomes cursed when it is unboxed.</b>")))

  __init__ (meth [&optional [boxed True] &kwargs rest]
    (apply Item.__init__ [@] rest)
    (set-self boxed)
    None)

  identified? (meth []
    ; Unboxed items count as identified, whether or not you've
    ; identified the box type.
    (if @boxed
      (.identified? (super Clothing @))
      True))

  applied (meth [] (block
    (unless @boxed
      (ret (.applied (super Clothing @))))
    (.take-time G.player @open-present-time)
    (setv was-id? (@identified?))
    (msg "You tear open {}." (if was-id?
      (.format "the box containing {:the}" @)
      (.format "{:the}" @)))
    (@identify)
    (setv @boxed False)
    (when @curse-on-unbox
      (@mk-curse))
    (unless was-id?
      (msg "You found:  {}" (self.invstr)))))

  carry-effects-active? (meth []
    (not @boxed)))

(defn pair-of [s]
  (kwc NounPhrase s :+always-plural :unit "pairs"))

(def-itemtype Clothing "sneakers" :name (pair-of "expensive sneakers")
  :color-fg :white
  :level-lo 4
  :rarity :uncommon
  :info-flavor "Guaranteed to make you run faster and jump higher! Nah, I lied. They only make you run faster. But that's more than can be said for PF Flyers and Cataclysm DDA."

  :carry-speed-factor-smooth-terrain 1.25
  :info-carry "You walk at {carry_speed_factor_smooth_terrain} times normal speed on smooth terrain.")

(def-itemtype Clothing "high-heels" :name (pair-of "fancy high heels")
  :color-fg :red
  :price-adj :bad-flavor
  :level-lo 4
  :info-flavor "Unless you're Ginger Rogers, these are going to make you somewhat less graceful."
    ; http://www.reelclassics.com/Actresses/Ginger/ginger-article2.htm

  :curse-on-unbox True
  :carry-speed-factor .8
  :info-carry "You walk at {carry_speed_factor} times normal speed.")

(def-itemtype Clothing "roller-skates" :name (pair-of "roller skates")
  :color-fg :yellow
  :level-lo 7
  :info-flavor "\"Aurelia, old girl,\" said Archibald Mulliner in a clear, firm voice, \"you are the bee's roller skates.\" And at that she seemed to melt into his embrace. Her lovely face was raised to his. \"Archibald!\" she whispered."

  :carry-speed-factor-smooth-terrain 2
  :carry-speed-factor-rough-terrain .5
  :info-carry "You walk at {carry_speed_factor_smooth_terrain} times normal speed on smooth terrain, but {carry_speed_factor_rough_terrain} times on rough terrain.")

(def-itemtype Clothing "cheb-boots" :name (pair-of "Chebyshev boots")
  :color-fg :dark-orange
  :level-lo 11
  :rarity :uncommon
  :info-flavor "This vintage Russian footwear makes you feel like a king."
    ; Pafnuty Chebyshev was Russian. Kings in chess move according
    ; to the Chebyshev metric.

  :carry-cheb-walk True
  :info-carry "When you walk, diagonal moves take the same amount of time as orthogonal moves.")

(def-itemtype Clothing "distressed-jeans" :name (pair-of "faux-distressed blue jeans")
  :color-fg :dark-blue
  :price-adj :bad-flavor
  :level-lo 2
  :info-flavor "These are favored by fashion fanatics, but walking around in pants full of holes and tears may also make you look like a yokel."

  :curse-on-unbox True
  :carry-gadget-malfunction-1in 20
  :info-carry "When you apply a gadget, there's a 1 in {carry_gadget_malfunction_1in} chance that nothing will happen, wasting a charge.")

(def-itemtype Clothing "fur-coat" :name "fancy fur coat"
  :color-fg :brown
  :level-lo 3
  :rarity :uncommon
  :info-flavor "A thick, luxurious coat made from the pelts of many adorable woodland creatures. When this prize was announced, animal-welfare organizations lambasted Rogue TV, calling for a boycott of the middlingly popular game show. This gave Rogue TV's ratings a much-needed boost."

  :carry-ice-immunity True
  :info-carry "You don't slip on ice.")

(def-itemtype Clothing "ugly-sweater" :name "ugly Christmas sweater"
  :color-fg :dark-green
  :rarity :uncommon
  :info-flavor "You can find this abomination in the dungeon all year round. It's your own ugly little Christmas in July. And, it's a gift that keeps on giving."

  :carry-gen-item Clothing
  :info-carry "Each time you enter a new level, an extra present is generated.")

(def-itemtype Clothing "lab-coat" :name "lab coat"
  :color-fg :white
  :info-flavor "With this groovy outerwear, you'll be chugging mystery sludge in no time."

  :carry-instant-soda-use True
  :info-carry "Removes the basic time cost of drinking sodas.")

(def-itemtype Clothing "trenchcoat"
  :color-fg :brown
  :level-lo 1
  :info-flavor "It's full of pockets for easy access to all your toys."

  :carry-instant-gadget-use True
  :info-carry "Removes the basic time cost of using gadgets.")

(def-itemtype Clothing "goggles" :name (pair-of "goggles")
  :color-fg :dark-red
  :price 1
  :level-lo 9
  :info-flavor "The goggles do nothing!"
    ; http://knowyourmeme.com/memes/the-goggles-do-nothing

  :curse-on-unbox True)

(setv (get ItemAppearance.registry Clothing) (amap
  (ItemAppearance (kwc NounPhrase
    (+ "present labeled " it)
    :plural (+ "presents labeled " it)
    :article "a"))
  gift-box-labels))
(assert (>= (get ItemAppearance.registry Clothing)
  (len (filt (instance? Clothing it) (.values G.itypes)))))
