(require kodhy.macros roguetv.macros)

(import
  [roguetv.globals :as G]
  [roguetv.types [Drawable MapObject]])

(defclass ItemType [Drawable] [

  [__init__ (fn [self tid name char &optional color-fg color-bg]
    (set-self tid name char)
    (set-self-nn color-fg color-bg)
    (setv (get G.itypes tid) self)
    None)]])

(defclass Item [MapObject] [

  [__init__ (fn [self itype &optional pos invlet]
    (.__init__ (super Item self) pos)
    (set-self itype invlet)
    None)]

  [invstr (fn [self]
    (.format "{} - {}"
      self.invlet
      self.itype.name))]])

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
