(require kodhy.macros roguetv.macros)

(import
  re
  [kodhy.util [cat keyword->str shift]]
  [roguetv.english [NounPhrase NounPhraseNamed]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [MapObject Generated Drawable]])

(defclass Item [MapObject Generated NounPhraseNamed Drawable] [
  [escape-xml-in-np-format True]
  [tid None]
    ; A string.
  [appearance None]
    ; An ItemAppearance.

  [info-unidentified "[Missing un-ID text]"]
  [info-flavor "[Missing flavor text]"]
  [info-apply None]
  [info-carry None]

  [price None]
    ; The money value of the item, a nonnegative integer.
    ; If not set explicitly, def-itemtype will set it.
  [price-adj None]
    ; A keyword that can adjust the price set by def-itemtype.

  [carry-speed-factor None]
    ; A floating-point number multiplying the player's speed
    ; when the item is carried.
  [superheavy False]
    ; If True, the player can't walk while carrying this item.

  [__init__ (fn [self &optional pos invlet]
    (Generated.__init__ self)
    (MapObject.__init__ self pos)
    (set-self invlet)
    None)]

  [set-appearance (classmethod (fn [self iapp]
    (setv self.appearance iapp)
    (setv self.color-fg iapp.color-fg)))]

  [identified? (fn [self]
    (not (and self.appearance (not self.appearance.known))))]

  [identify (fn [self &optional [consumed False]]
    (unless (.identified? self)
      (setv self.appearance.known True)
      (when (in self G.inventory)
        (msg "You {}:  {}"
          (if consumed "had" "have")
          (self.invstr)))))]

  [__format__ (fn [self formatstr]
    ; Examples:
    ;  "{}"               hookshot
    ;  "{:the}"           the hookshot
    ;  "{:the:full}"      the hookshot (10) [$5] (see Item.name-suffix)
    ;  "{:the:most}"      the hookshot (10)
    ;  "{::full}"         hookshot (10) [$5]
    ;  "{:the:true}"      the hookshot           (even if the player hasn't identified the hookshot yet)
    ;  "{:the:true,full}" the hookshot (10) [$5] (ditto)
    (setv [np-args tags] (.groups (re.match
      "( (?: v:)? [^:]* )  (?: : (.+) )?"
      formatstr
      re.VERBOSE)))
    (setv tags (set (if tags (.split tags ",") [])))
    (setv name (if (in "true" tags) self.name (self.apparent-name)))
    (.escape self (kwc cat :sep " " (.__format__ name np-args)
      (when (or (in "most" tags) (in "full" tags)) (kwc cat :sep " "
        (self.name-suffix)
        (when (in "full" tags)
          (.format "[${}]" (self.apparent-price))))))))]

  [information (fn [self]
    (.format "\n  {} {:a:full}\n\n{}"
      (.xml-symbol self)
      self
      (apply .format
        [(if (.identified? self)
          (.join "\n\n" (+
            [self.info-flavor]
            (if self.info-apply [(+ "<b>Effect when applied:</b> " self.info-apply)] [])
            (if self.info-carry [(+ "<b>Effect when carried:</b> " self.info-carry)] [])))
          self.info-unidentified)]
        (. (type self) __dict__))))]

  [apparent-name (fn [self]
    (if (.identified? self)
      self.name
      self.appearance.name))]

  [apparent-price (fn [self]
    (if (.identified? self)
      self.price
      "?"))]

  [invstr (fn [self]
    (.format "{} {} {:a:full}"
      self.invlet
      (.xml-symbol self)
      self))]

  [name-suffix (fn [self]
    ; This method can be overridden to provide extra information
    ; about an item, like the number of charges. It's only displayed
    ; with the "full" or "most" formatting tags.
    None)]

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
  (setv (get (globals) c.__name__) c)
    ; This ensures that jsonpickle can recreate itypes.
  (setv (get G.itypes tid) c)

  (setv c.tid tid)
  (when (none? c.name)
    (setv c.name c.tid))
  (setv c.name (NounPhrase c.name))
  (when (none? c.price) (setv c.price (*
    (+ c.level-lo 2)
    (ecase c.price-adj
      [None         1]
      [:bad-flavor  2]
        ; Items that are flavored items (e.g., gadgets) and
        ; have generally bad effects are worth more, so they
        ; can still be valuable to the player.
      [:burden      4])
        ; Items that are high-value but worse than useless.
    (ecase c.rarity
      [:common   1]
      [:uncommon 2]
      [:rare     3]))))

  c)

(defclass ItemAppearance [NounPhraseNamed] [
  [registry {}]
  ; A dictionary mapping subclasses of Item to lists of eligible
  ; appearances.

  [__init__ (fn [self name color-fg]
    (set-self name color-fg)
    (setv self.known False)
      ; .known is true when the player has learned the type of
      ; item that goes with this appearance.
    None)]

  [randomize-appearances (classmethod (fn [self]
     (setv unused-apps (dict (lc
       [[c apps] (.items self.registry)]
       (, c (list apps)))))
     (for [itype (.values G.itypes)]
       (whenn (afind-or (issubclass itype it) (.keys unused-apps))
         (.set-appearance itype (randpop (get unused-apps it)))))))]])


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
