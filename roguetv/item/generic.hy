(require kodhy.macros roguetv.macros)

(import
  [math [ceil]]
  re
  [kodhy.util [cat keyword->str shift ret]]
  [roguetv.english [NounPhrase NounPhraseNamed]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [MapObject Generated Drawable Scheduled]]
  [roguetv.map [Tile room-for?]])

(defclass Item [MapObject Generated Scheduled NounPhraseNamed Drawable] [
  [escape-xml-in-np-format True]
  [tid None]
    ; A string.
  [appearance None]
    ; An ItemAppearance.

  [info-unidentified "[Missing un-ID text]"]
  [info-flavor "[Missing flavor text]"]
  [info-apply None]
  [info-carry None]
  [info-constant None]

  [price None]
    ; The money value of the item, a nonnegative integer.
    ; If not set explicitly, def-itemtype will set it.
  [price-adj None]
    ; A keyword that can adjust the price set by def-itemtype.

  [indestructible False]
    ; Indestructible items can't be destroyed by, e.g., paper
    ; shredders.
  [carry-speed-factor None]
    ; A floating-point number multiplying the player's speed
    ; when the item is carried.
  [carry-speed-factor-smooth-terrain None]
    ; Like .carry-speed-factor, but applies only when exiting
    ; smooth terrian.
  [carry-speed-factor-rough-terrain None]
    ; Like .carry-speed-factor, but applies only when exiting
    ; non-smooth terrian.
  [superheavy False]
    ; If True, the player can't walk while carrying this item.
  [carry-ice-immunity False]
    ; If True, the player is immune to ice.
  [carry-cheb-walk False]
    ; If True, the player walks according to the Chebyshev metric.
  [carry-gadget-malfunction-1in None]
    ; A chance of gadgets malfunctioning when they're applied.
  [carry-instant-gadget-use False]
    ; Allows the player to apply gadgets without the usual time cost.
  [carry-instant-soda-use False]
    ; Allows the player to apply sodas without the usual time cost.
  [carry-gen-item None]
    ; If a class, one of that type of item is generated on each
    ; new level.
  [carry-gen-monster None]
    ; If a type, one of that type of monster is generated on
    ; each new level. Unlike carry-gen-item, the type is used
    ; directly as the constructor, rather than filtering the
    ; types that could be generated for this level.
  [carry-repel-monster None]
    ; If a class, monsters of that type will flee from the
    ; player.

  [__init__ (fn [self &kwargs kw]
    (Generated.__init__ self)
    (MapObject.__init__ self (.get kw "pos"))
    (setv self.invlet None)
    (setv self.curse None)
      ; Cursed items can't be dropped.
    None)]

  [clone-setup (fn [self orig]
    (when orig.curse
      (setv self.curse (.clone orig.curse self))))]

  [clone (fn [self &optional pos]
    (setv new (kwc (type self) :pos pos))
    (.clone-setup new self)
    new)]

  [destroy (fn [self]
    ; For cleaning up after an item that no longer exists. For
    ; the game effect of destroying an item (which may call this
    ; method), see the `delete` method.
    (when self.curse
      (.destroy self.curse))
    (.destroy (super Item self)))]

  [get-color-fg (fn [self]
    (if (.identified? self)
      (.get-color-fg (super Item self))
      G.unid-item-color))]

  [set-appearance (classmethod (fn [self iapp]
    (setv self.appearance iapp)))]

  [identified? (fn [self]
    (not (and self.appearance (not self.appearance.known))))]

  [identify (fn [self]
    (unless (.identified? self)
      (setv self.appearance.known True)))]

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
        (when self.curse "(cursed)")
        (when (in "full" tags)
          (.format "[${}]" (self.apparent-price))))))))]

  [information (fn [self]
    (.format "\n  {} {:a:full}\n\n{}"
      (.xml-symbol self)
      self
      (apply .format
        [(if (.identified? self)
          (kwc cat :sep "\n\n"
            self.info-flavor
            (when self.unique "<b>This item is unique.</b>")
            (when self.indestructible "<b>This item is indestructible.</b>")
            (when self.curse "<b>This item is cursed,</b> preventing you from dropping it. The curse will eventually go away on its own.")
            (self.info-extra)
            (when self.info-apply (+ "<b>Effect when applied:</b> " self.info-apply))
            (when self.info-carry (+ "<b>Effect when carried:</b> " self.info-carry))
            (when self.info-constant (+ "<b>Constant effect:</b> " self.info-constant)))
          self.info-unidentified)]
        (dict (lc [[k v] (.items (. (type self) __dict__))] (,
          (.replace k "_" "-")
          (if (.endswith k "_time") (show-duration v) v)))))))]

  [info-extra (fn [self]
    None)]

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

  [delete (fn [self] (block
    (when self.indestructible
      (ret False))
    (setv where (find-item self))
    (cond
      [(instance? Pos where)
        (.move self None)]
      [(is where G.player)
        (.remove G.inventory self)]
      [True (do
        (assert (hasattr where "item"))
        (setv where.item None))])
    (.destroy self)
    True))]

  [mk-curse (fn [self]
    (setv self.curse (Curse self)))]

  [name-suffix (fn [self]
    ; This method can be overridden to provide extra information
    ; about an item, like the number of charges. It's only displayed
    ; with the "full" or "most" formatting tags.
    None)]

  [applied (fn [self]
    ; This is triggered when the player uses the :apply-item command.
    (msg "You can't do anything special with {:the}." self))]

  [carry-effects-active? (fn [self]
    True)]

  [on-reset-level (fn [self]
    ; This is triggered when the level is reset for each item
    ; in the player's inventory.
    None)]])

(defn def-itemtype [inherit tid &rest body]

  (when (in tid G.itypes)
    (raise (ValueError (.format "redeclared item type: {}" tid))))

  (setv attrdict (dict (amap
    (let [[k (get body (* 2 it))] [v (get body (inc (* 2 it)))]]
      (, (.replace (keyword->str k) "-" "_") v))
    (range (// (len body) 2)))))
  (setv c (type
    (str (+ "itype:" tid))
    (if (instance? list inherit) (tuple inherit) (, inherit))
    attrdict))
  (setv (get (globals) c.__name__) c)
    ; This ensures that jsonpickle can recreate itypes.
  (setv (get G.itypes tid) c)

  (setv c.tid tid)
  (when (not-in "name" attrdict)
    (setv c.name c.tid))
  (setv c.name (NounPhrase c.name))
  (when (not-in "price" attrdict)
    (setv p (*
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
        [:rare     3])))
    (setv round-up-to (cond
      [(<= p  15)   1]
      [(<= p  50)   5]
      [(<= p 150)  10]
      [(<= p 500)  50]
      [True       100]))
    (setv c.price (long (* round-up-to (ceil (/ p round-up-to))))))

  c)

(defclass ItemAppearance [NounPhraseNamed] [
  [registry {}]
  ; A dictionary mapping subclasses of Item to lists of eligible
  ; appearances.

  [__init__ (fn [self apid name]
    ; `apid` is a short string identifying the appearance, whereas
    ; `name` is a NounPhrase.
    (set-self apid name)
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

(defcls Curse [Scheduled]
  curse-fade-time (meth []
    (randexp-dl-div 1))

  __init__ (meth [host-item]
    (set-self host-item)
    (@schedule)
    (@take-time (@curse-fade-time))
    None)

  clone (meth [new-host-item]
    ; Ignore the curse-fade-time of the original. Just make
    ; a new curse.
    (Curse new-host-item))

  remove-curse (meth []
    (setv @host-item.curse None)
    (when (in @host-item G.inventory)
      (msg :tara "{p:}, the curse on {:your} has faded." @host-item))
    (@deschedule))

  act (meth []
    (@remove-curse)))

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

(defn drop-pos [p]
  ; Try to find a position near 'p' to drop an item.
  (afind-or (and (room-for? Item it) (not (. (Tile.at it) container))) (+
    ; Try to drop at 'p'…
    [p]
    ; …or at a random orthogonal neigbor…
    (shuffle (amap (+ p it) Pos.ORTHS))
    ; …or at a random diagonal neighbor.
    (shuffle (amap (+ p it) Pos.DIAGS)))))

(defn find-item [item]
; Find where an item is, even if it's not on the ground.
  (cond
    [item.pos
      item.pos]
    [(in item G.inventory)
      G.player]
    [True
      (afind
        (and
          (instance? (rtv-get creature.monster.Nymph) it)
          (is it.item item))
        (rtv creature.monster.extant-monsters))]))

(defn item-pos [item]
  (setv where (find-item item))
  (if (instance? Pos where)
    where
    ; Otherwise, 'where' should be a creature.
    where.pos))
