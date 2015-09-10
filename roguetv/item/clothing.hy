(require kodhy.macros roguetv.macros)

(import
  [kodhy.util [ret]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.item.generic [Item ItemAppearance def-itemtype]])

(defcls Clothing [Item]
  open-present-time 1
  char "["

  info-unidentified "This festively wrapped gift box contains an item of clothing. Your only clue as to what's inside is a cryptic product code. Clothing has a special effect on you so long you carry it, but the effect is suppressed while it's in a box. 'a'pply the box to open it."

  name-suffix (meth []
    (when (and (@identified?) @boxed)
      "(boxed)"))

  info-extra (meth []
    (when @boxed
      "<b>This item is boxed</b>, suppressing its carry effect. 'a'pply it to open and discard the box."))

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
    (unless was-id?
      (msg "You found:  {}" (self.invstr)))))

  carry-effects-active? (meth []
    (not @boxed)))

(def appearances {
  "1.21GW"		:blue  ; Back to the Future
  "1729"		:blue  ; The Hardy–Ramanujan number
  "18156248"		:blue  ; PMID of "Sex, aggression, and humour: responses to unicycling"
  "2038019"		:blue  ; 2038-01-19 is the Unix doomsday
  "23.36664"		:blue  ; sqrt(546), from MLP's Failure Song
  "25212683"		:blue  ; PMID of my first published article
  "2FA3E1B"		:blue  ; Initial commit of Rogue TV
  "32W353Y"		:blue  ; 0xDEADBEEF in RFC 4648 Base32
  "41421356"		:blue  ; sqrt(2)
  "6ACCDAE13EFF7I3L"	:blue  ; First half of Newton's anagram
  "794.8"		:blue  ; Dewey Decimal for computer games
  "9N4O4QRR4S8T12UX"	:blue  ; Second half of Newton's anagram
  "AA23C2187"		:blue  ; Princess Leia was in detention block AA-23, cell 2187
  "ARGELFRASTER"	:blue  ; Enchanted Forest Chronicles
  "BIDOOF"		:blue  ; A Pokémon
  "BLINKENLICHTEN"	:blue  ; http://www.catb.org/jargon/html/B/blinkenlights.html
  "CHIM"		:blue  ; http://www.uesp.net/wiki/Lore:CHIM
  "COSMICOSMO"		:blue  ; Cosmic Osmo
  "DOOTDOOT"		:blue  ; http://knowyourmeme.com/memes/skull-trumpet
  "E102G"		:blue  ; E-102 Gamma
  "FRINDLE"		:blue  ; The book of the same name
  "GOOZACK"		:blue  ; Wayside School
  "IDDQD"		:blue  ; Doom cheat code
  "KBA" 		:blue  ; My initials
  "MUGWORMGRIBLICK"	:blue  ; Wayside School
  "OOSMNSPFRSL" 	:blue  ; Abbreviation for The Origin of Species
  "OYGEVALT"		:blue  ; Oy gevalt
  "PYSZCZYNSKI" 	:blue  ; Tom Pyszczynski, TMT researcher
  "QLZQQLZUUP"		:blue  ; The Emperor Quylthulg in Angband
  "SLITHYTOVES" 	:blue  ; Jabberwocky
  "SPAM"		:blue
  "SPISPOPD"		:blue  ; http://doom.wikia.com/wiki/SPISPOPD
  "TIBYOCSPNLAAD"       :blue  ; https://www.reddit.com/r/OutOfTheLoop/comments/1w7ojb
  "TVERSKY"		:blue  ; Amos Tversky, JDM researcher
  "UWOTM8"		:blue  ; http://knowyourmeme.com/memes/u-wot-m8
  "X3J13"		:blue  ; Common Lisp standardization committee
  "YAGRUMBAGARN"	:blue  ; http://www.uesp.net/wiki/Morrowind:Yagrum_Bagarn
  "ZOOMBINI"		:blue  ; Zoombinis video-game series
})
(setv (get ItemAppearance.registry Clothing) (lc
  [[name color] (.items appearances)]
  (kwc ItemAppearance
    :name (kwc NounPhrase (+ "present labeled " name) :article "a")
    :color-fg color)))

(defn pair-of [s]
  (kwc NounPhrase s :+mass :unit "pairs"))

(def-itemtype Clothing "sneakers" :name (pair-of "expensive sneakers")
  :level-lo 4
  :rarity :uncommon
  :info-flavor "Guaranteed to make you run faster and jump higher! Nah, I lied. They only make you run faster. But that's more than can be said for PF Flyers and Cataclysm DDA."

  :carry-speed-factor-smooth-terrain 1.25
  :info-carry "You walk {carry_speed_factor_smooth_terrain} times normal speed on smooth terrain.")

(def-itemtype Clothing "roller-skates" :name (pair-of "roller skates")
  :level-lo 7
  :info-flavor "\"Aurelia, old girl,\" said Archibald Mulliner in a clear, firm voice, \"you are the bee's roller skates.\" And at that she seemed to melt into his embrace. Her lovely face was raised to his. \"Archibald!\" she whispered."

  :carry-speed-factor-smooth-terrain 2
  :carry-speed-factor-rough-terrain .5
  :info-carry "You walk {carry_speed_factor_smooth_terrain} times normal speed on smooth terrain, but {carry_speed_factor_rough_terrain} times on rough terrain.")

(assert (>= (len appearances)
  (len (filt (instance? Clothing it) (.values G.itypes)))))
