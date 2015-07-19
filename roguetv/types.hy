(require kodhy.macros)

(import
  xml.sax.saxutils
  [roguetv.globals :as G]
  [roguetv.util [*]])

(defclass Drawable [object] [
  [char None]
  [color-fg G.fg-color]
  [color-bg None]

  [xml-symbol (fn [self]
    (color-xml
      (xml.sax.saxutils.escape self.char)
      self.color-fg
      self.color-bg))]])

(defclass MapObject [object] [

  [init-omap (classmethod (fn [self width height]
    (setv self.omap (amap (* [None] height) (range width)))))]

  [__init__ (fn [self &optional pos]
    ; 'pos' may be None whenever the object isn't currently
    ; on the map.
    (setv self.pos None)
    (.move self pos)
    None)]

  [move (fn [self p-to &optional [clobber False]]
    ; Set 'p-to' to None to remove the object from the map.
    ;
    ; If 'p-to' is not None, 'clobber' is true, and there's
    ; something already at 'p-to', remove it. Otherwise, moving
    ; onto a Pos where there's already something else is an
    ; error.
    (when self.pos
      (try
        (setv (get self.omap self.pos.x self.pos.y) None)
        (catch [_ IndexError])))
          ; An IndexError may arise from the assignment if we've
          ; done a .move after an old position has become
          ; invalid. This is fine.
    (when p-to
      (whenn (get self.omap p-to.x p-to.y)
        (if clobber
          (it.move None)
          (raise (ValueError (.format
            "tried to move {} to {} where there was already {}"
            self p-to it)))))
      (setv (get self.omap p-to.x p-to.y) self))
    (setv self.pos p-to))]

  [at (classmethod (fn [self pos]
    (get self.omap pos.x pos.y)))]])

(defcls Generated [object]
  level-lo 0
  level-hi G.max-dungeon-level
  rarity :common

  generation-weight (cmeth [dl]
    (if (= @rarity :nongen) 0 (*
      (/ 1 (cond
        [(= @rarity :common)    1]
        [(= @rarity :uncommon)  4]
        [(= @rarity :rare)     16]))
      (/ 1 (cond
        [(< dl @level-lo) (inc (- @level-lo dl))]
        [(> dl @level-hi) (inc (- dl @level-hi))]
        [True 1]))))))
