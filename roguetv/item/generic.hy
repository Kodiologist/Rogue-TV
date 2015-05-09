(require kodhy.macros roguetv.macros)

(import
  [kodhy.util [keyword->str shift]]
  [roguetv.english [NounPhrase NounPhraseNamed]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Drawable MapObject]])

(defclass Item [Drawable MapObject NounPhraseNamed] [
  [tid None]
    ; A string.
  [appearance None]
    ; An ItemAppearance.

  [__init__ (fn [self &optional pos invlet]
    (MapObject.__init__ self pos)
    (set-self invlet)
    None)]

  [set-appearance (classmethod (fn [self iapp]
    (setv self.appearance iapp)
    (setv self.color-fg iapp.color-fg)))]

  [apparent-name (fn [self]
    (if (and self.appearance (not self.appearance.known))
      self.appearance.name
      self.name))]

  [display-name (fn [self]
    (. (.apparent-name self) indefinite-singular))]

  [invstr (fn [self]
    (.format "{} - {}"
      self.invlet
      (self.display-name)))]

  [applied (fn [self cr]
    ; This is triggered by, e.g., the :apply-item command.
    (when (is cr G.player)
      (msgn "You can't do anything special with {.definite_singular}."
        (self.apparent-name))))]])

(defn def-itemtype [inherit tid &rest body]

  (when (in tid G.itypes)
    (raise (ValueError (.format "redeclared item type: {}" tid))))

  (defclass C [inherit] [])
  (setv (get G.itypes tid) C)
  (setv C.tid tid)

  (setv body (list body))
  (while body
    (setattr C
      (.replace (keyword->str (shift body)) "-" "_")
      (shift body)))

  (when (none? C.name)
    (setv C.name C.tid))
  (setv C.name (NounPhrase C.name))

  C)

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
