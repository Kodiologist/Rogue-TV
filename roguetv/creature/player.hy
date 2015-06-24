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
  [info-text "Applying state-of-the-art neuroludological algorithms to the choices you've made while playing this game, I've inferred the following about you:\n\nYou have a great need for other people to like and admire you. You have a tendency to be critical of yourself. You have a great deal of unused capacity which you have not turned to your advantage. While you have some personality weaknesses, you are generally able to compensate for them. Your sexual adjustment has presented problems for you. Disciplined and self-controlled outside, you tend to be worrisome and insecure inside. At times you have serious doubts as to whether you have made the right decision or done the right thing. You prefer a certain amount of change and variety and become dissatisfied when hemmed in by restrictions and limitations. You pride yourself as an independent thinker and do not accept others' statements without satisfactory proof. You have found it unwise to be too frank in revealing yourself to others."]
    ; Forer, B. R. (1949). The fallacy of personal validation: A classroom demonstration of gullibility. Journal of Abnormal Psychology, 44(1), 118–123. doi:10.1037/h0059240

  [can-open-doors True]

  [__init__ (fn [self &optional pos]
    (Creature.__init__ self pos)
    (setv self.stink-until -1)
      ; The player is considered stinky until (>= G.current-time G.player.stink-time).
    None)]

  [move (fn [self p-to &optional [clobber False]]
    (.move (super Player self) p-to clobber)
    (soil-fov))]

  [act (fn [self]
    (full-redraw)
    (setv old-clock-debt self.clock-debt-ms)
    (do-normal-command (get-normal-command))
    (when (and
        (!= self.stink-until -1)
        (>= G.current-time self.stink-until))
      (msg "You smell presentable again.")
      (setv self.stink-until -1))
        ; Just so this message doesn't print again.
    (setv G.last-action-duration (/
      (- self.clock-debt-ms old-clock-debt)
      Creature.clock-factor)))]])
