(import
  curses
  [libtcodpy :as tcod])

(def globals-to-save (set))
(defmacro defs [varname value]
  ; Define a variable and mark it to be saved.
  (import [hy [HyString]])
  `(do
    (def ~varname ~value)
    (.add globals-to-save ~(HyString varname))))

;; * Parameters

(def debug False)

(defs map-width 80)
(defs map-height 40)

(defs inventory-limit 10)

(def message-lines 3)
(def map-border-width 2)
(def text-screen-left-margin 1)

(def fg-color :black)
(def bg-color :white)
(def pick-bg-color (fn [fg]
  (if (in fg [bg-color :yellow])
    :dark-gray
    bg-color)))
(def unseen-color :dark-gray)
(def off-map-color :very-dark-gray)
(def announcer-colors {
  :tara :blue
  :bob :red
  :aud :dark-green})

(def new-msg-highlight curses.A-BOLD)

(def key-escape "\x1b")

(defs invlets (list "abcdefghijklmnopqrstuvwxyz"))

(def color-numbers {
  :black 16
  :white 15
  :dark-gray 244
  :very-dark-gray 235
  :red 196
  :dark-red 88
  :green 46
  :dark-green 22
  :blue 21
  :dark-blue 18
  :pale-azure 117
  :purple 90
  :yellow 226
  :orange 208
  :dark-orange 166
  :brown 94})

(defs push-past-monster-time 1)
(defs stink-range 20)
(defs speedup-soda-factor 2)

;; * Declarations

(defs save-file-path None)
(defs dates {
  "started" None
  "saved" None
  "loaded" None})

(def look-mode-legend-height 4)

(def bottom-border (max look-mode-legend-height (+ message-lines 1)))
  ; The extra 1 is for the status line.

(def T None) ; This will be set to a curses screen.
(def screen-width None)
(def screen-height None)
(def color-pairs {})
(def locale-encoding None)
(defs message-log [])
(defs last-new-message-number -1)

(def screen-mode None)

(defs player None)

(def endgame False)
(defs dungeon-level None)

; Times are in simulated seconds.
(defs current-time 0)
(defs time-limit None)
(defs last-action-duration 0)

(def fov-map (tcod.map-new map-width map-height))
(def fov-dirty? True)
(defs seen-map [])

(defs inventory [])
(def itypes {})
