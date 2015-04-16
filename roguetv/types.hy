(require kodhy.macros)

(import
  [roguetv.globals :as G])

(defclass Drawable [object] [
  [char None]
  [color-fg G.fg-color]
  [color-bg G.bg-color]])

(defclass MapObject [object] [

  [init-omap (classmethod (fn [self width height]
    (setv self.omap (amap (* [None] height) (range width)))))]

  [__init__ (fn [self &optional pos]
    ; 'pos' may be None whenever the object isn't currently
    ; on the map.
    (setv self.pos None)
    (.move self pos)
    None)]

  [move (fn [self p-to]
    ; Set p-to to None to remove the object from the map.
    (when self.pos
      (setv (get self.omap self.pos.x self.pos.y) None))
    (when p-to
      (setv (get self.omap p-to.x p-to.y) self))
    (setv self.pos p-to))]

  [at (classmethod (fn [self pos]
    (get self.omap pos.x pos.y)))]])
