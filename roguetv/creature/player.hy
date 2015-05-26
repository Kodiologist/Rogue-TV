(require kodhy.macros)

(import
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [get-normal-command]]
  [roguetv.creature [Creature]]
  [roguetv.display [full-redraw]]
  [roguetv.actions [do-normal-command]])

(defclass Player [Creature] [
  [char "@"]
  [color-bg :yellow]

  [can-open-doors True]

  [move (fn [self p-to &optional [clobber False]]
    (.move (super Player self) p-to clobber)
    (soil-fov))]

  [act (fn [self]
    (full-redraw)
    (setv old-clock-debt G.player.clock-debt-ms)
    (do-normal-command (get-normal-command))
    (setv G.last-action-duration (/
      (- G.player.clock-debt-ms old-clock-debt)
      Creature.clock-factor)))]])
