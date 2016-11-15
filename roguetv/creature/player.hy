(require [kodhy.macros [amap afind-or]])

(import
  [kodhy.util [ret product]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [get-normal-command]]
  [roguetv.map [Tile]]
  [roguetv.creature [Creature Haste]]
  [roguetv.display [full-redraw]]
  [roguetv.actions [do-normal-command]])

(defclass Player [Creature] [
  queue-priority -1

  char "@"
  color-bg :yellow
  info-text "Applying state-of-the-art neuroludological algorithms to the choices you've made while playing this game, I've inferred the following about you:\n\nYou have a great need for other people to like and admire you. You have a tendency to be critical of yourself. You have a great deal of unused capacity which you have not turned to your advantage. While you have some personality weaknesses, you are generally able to compensate for them. Your sexual adjustment has presented problems for you. Disciplined and self-controlled outside, you tend to be worrisome and insecure inside. At times you have serious doubts as to whether you have made the right decision or done the right thing. You prefer a certain amount of change and variety and become dissatisfied when hemmed in by restrictions and limitations. You pride yourself as an independent thinker and do not accept others' statements without satisfactory proof. You have found it unwise to be too frank in revealing yourself to others."
    ; Forer, B. R. (1949). The fallacy of personal validation: A classroom demonstration of gullibility. Journal of Abnormal Psychology, 44(1), 118–123. doi:10.1037/h0059240

  can-open-doors True

  push-past-monster-time (seconds 1)
  confusion-bump-time (seconds 1)
  take-item-time (seconds 1)
  drop-item-time (seconds 1)

  __init__ (fn [self &optional pos]
    (Creature.__init__ self pos)
    (setv self.effects [])
    (setv self.just-slept False)
    (setv self.last-turn 0)
    None)

  ice-immune (fn [self]
    (afind-or it.carry-ice-immunity (active-inv)))

  move (fn [self p-to &optional [clobber False]]
    (.move (super Player self) p-to clobber)
    (soil-fov))

  walk-dist (fn [self p-from p-to]
    (if (afind-or it.carry-cheb-walk (active-inv))
      (dist-cheb p-from p-to)
      (.walk-dist (super Player self) p-from p-to)))

  walk-speed (fn [self]
    (if G.super-speed 1e6 (product (+
      (amap (or it.carry-speed-factor 1) (active-inv))
      (amap
        (or
          (if (. (Tile.at G.player.pos) smooth)
            it.carry-speed-factor-smooth-terrain
            it.carry-speed-factor-rough-terrain)
          1)
        (active-inv))
      [(if (.get-effect self Haste)
        G.haste-factor
        1)]))))

  act (fn [self]
    (when self.just-slept
      (setv self.just-slept False)
      (msg "You wake up."))
    (setv G.last-action-duration
      (- G.current-time self.last-turn))
    (full-redraw)
    (setv self.last-turn G.current-time)
    (do-normal-command (get-normal-command)))

  get-effect (fn [self effect-cls]
    (afind-or (instance? effect-cls it) self.effects))

  fall-asleep (fn [self sleep-time]
    (.take-time self sleep-time)
    (setv self.just-slept True))])
