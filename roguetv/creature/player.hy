(require kodhy.macros)

(import
  [kodhy.util [ret]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [get-normal-command]]
  [roguetv.map [Tile room-for? circ-taxi]]
  [roguetv.creature [Creature Haste Sleep Passwall]]
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
    (setv self.effects [])
    None)]

  [move (fn [self p-to &optional [clobber False]]
    (.move (super Player self) p-to clobber)
    (soil-fov))]

  [walk-speed (fn [self]
    (if (.has-effect self Haste)
      G.speedup-soda-factor
      1))]

  [act (fn [self]
    (for [e self.effects]
      (when (>= G.current-time e.expiry)
        (msg e.end-msg)
        (.remove self.effects e)
        (when (and (instance? Passwall e) (. (Tile.at self.pos) blocks-movement))
          (block
            ; Passwall just ended while the player was in a solid
            ; obstacle. Eject them to the nearest clear tile.
            (for [r (seq 1 (+ G.map-width G.map-height))]
              (for [p (shuffle (circ-taxi self.pos r))]
                (when (room-for? Creature p)
                  (msg "As you materialize, you are ejected from {:the}." (Tile.at self.pos))
                  (.move self p)
                  (ret))))
            ; There's no room anywhere on the level!
            (msg :tara "Oh no! Is {p:the} trapped inside {:the}?" (Tile.at self.pos))))))
    (full-redraw)
    (setv old-clock-debt self.clock-debt-ms)
    (if (.has-effect self Sleep)
      (.wait self)
      (do-normal-command (get-normal-command)))
    (setv G.last-action-duration (/
      (- self.clock-debt-ms old-clock-debt)
      Creature.clock-factor)))]

  [has-effect (fn [self effect-cls]
    (setv e (afind-or (instance? effect-cls it) self.effects))
    (and e (< G.current-time e.expiry)))]

  [add-effect (fn [self effect-cls duration start-msg lengthen-msg]
    (setv expiry (+ G.current-time duration))
    (setv e (afind-or (instance? effect-cls it) self.effects))
    ; If the player already has an effect of this kind,
    ; refresh the duration to the maximum of the old one and the
    ; new one.
;    (raise (ValueError [e effect-cls duration start-msg lengthen-msg]))
    (if e
      (do
        (lengthen-msg)
        (setv e.expiry (max e.expiry expiry)))
      (do
        (start-msg)
        (.append self.effects (effect-cls expiry)))))]])
