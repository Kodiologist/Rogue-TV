(require kodhy.macros roguetv.macros)

(import
  [kodhy.util [keyword->str shift]]
  [roguetv.globals :as G]
  [roguetv.types [Drawable MapObject]])

(defclass Item [Drawable MapObject] [
  [tid None]
  [name None]

  [__init__ (fn [self &optional pos invlet]
    (MapObject.__init__ self pos)
    (set-self invlet)
    None)]

  [invstr (fn [self]
    (.format "{} - {}"
      self.invlet
      self.name))]])

(defn def-itemtype [tid name &rest body]
  (when (in tid G.itypes)
    (raise (ValueError (.format "redeclared item type: {}" tid))))
  (defclass C [Item] [])
  (setv (get G.itypes tid) C)
  (setv C.tid tid)
  (setv C.name name)
  (setv body (list body))
  (while body
    (setattr C
      (.replace (keyword->str (shift body)) "-" "_")
      (shift body)))
  C)

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
