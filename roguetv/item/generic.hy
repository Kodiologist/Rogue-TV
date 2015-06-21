(require kodhy.macros roguetv.macros)

(import
  re
  [kodhy.util [cat keyword->str shift]]
  [roguetv.english [NounPhrase NounPhraseNamed]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Drawable MapObject]])

(defclass Item [Drawable MapObject NounPhraseNamed] [
  [tid None]
    ; A string.
  [appearance None]
    ; An ItemAppearance.

  [info-unidentified "[Missing un-ID text]"]
  [info-flavor "[Missing flavor text]"]
  [info-apply None]
  [info-carry None]

  [__init__ (fn [self &optional pos invlet]
    (MapObject.__init__ self pos)
    (set-self invlet)
    None)]

  [set-appearance (classmethod (fn [self iapp]
    (setv self.appearance iapp)
    (setv self.color-fg iapp.color-fg)))]

  [__format__ (fn [self formatstr]
    ; Examples:
    ;  "{}"               hookshot
    ;  "{:the}"           the hookshot
    ;  "{:the:full}"      the hookshot (10)  (see Item.name-suffix)
    ;  "{::full}"         hookshot (10)
    ;  "{:the:true}"      the hookshot       (even if the player hasn't identified the hookshot yet)
    ;  "{:the:true,full}" the hookshot (10)  (ditto)
    (setv [article tags] (.groups (re.match
      "( [^:]* )  (?: : (.+) )?"
      formatstr
      re.VERBOSE)))
    (setv tags (set (if tags (.split tags ",") [])))
    (setv name (if (in "true" tags) self.name (self.apparent-name)))
    (cat (.format-nounphrase self name article)
      (when (in "full" tags) (self.name-suffix))))]

  [information (fn [self]
    (setv s (.format "\n  {:a:full}\n\n{}"
      self
      (if (and self.appearance (not self.appearance.known))
        self.info-unidentified
        (.join "\n\n" (+
          [self.info-flavor]
          (if self.info-apply [(+ "Effect when applied: " self.info-apply)] [])
          (if self.info-carry [(+ "Effect when carried: " self.info-carry)] []))))))
    (apply .format [s] (. (type self) __dict__)))]

  [apparent-name (fn [self]
    (if (and self.appearance (not self.appearance.known))
      self.appearance.name
      self.name))]

  [invstr (fn [self]
    (.format "{} - {:a:full}"
      self.invlet
      self))]

  [name-suffix (fn [self]
    ; This method can be overridden to provide extra information
    ; about an item, like the number of charges. It's only displayed
    ; with the "full" formatting tag.
    "")]

  [applied (fn [self]
    ; This is triggered when the player uses the :apply-item command.
    (msg "You can't do anything special with {:the}." self))]

  [on-reset-level (fn [self]
    ; This is triggered when the level is reset for each item
    ; in the player's inventory.
    None)]])

(defn def-itemtype [inherit tid &rest body]

  (when (in tid G.itypes)
    (raise (ValueError (.format "redeclared item type: {}" tid))))

  (setv c (type (str (+ "itype:" tid)) (, inherit) (dict (amap
    (let [[k (get body (* 2 it))] [v (get body (inc (* 2 it)))]]
      (, (.replace (keyword->str k) "-" "_") v))
    (range (// (len body) 2))))))
  (setv (get G.itypes tid) c)

  (setv c.tid tid)
  (when (none? c.name)
    (setv c.name c.tid))
  (setv c.name (NounPhrase c.name))

  c)

(def-itemtype Item "test-item"
  :name "test item" :name-suffix (fn [self] " (testy)")
  :char "&"
  :info-flavor "This is a test item. It doesn't do anything.")

(defclass ItemAppearance [NounPhraseNamed] [

  [__init__ (fn [self name color-fg]
    (set-self name color-fg)
    (setv self.known False)
      ; .known is true when the player has learned the type of
      ; item that goes with this appearance.
    None)]])

(defn add-to-inventory [item]
  (.move item None)
  (setv il-in-use (amap it.invlet G.inventory))
  (when (or (not item.invlet) (in item.invlet il-in-use))
    ; Assign the oldest invlet not used for an item already in
    ; the inventory.
    (setv item.invlet (afind-or (not-in it il-in-use) G.invlets))
    ; Move this invlet to the end of 'invlets' (since it's now
    ; the most recently used).
    (G.invlets.remove item.invlet)
    (G.invlets.append item.invlet))
  (.append G.inventory item))
