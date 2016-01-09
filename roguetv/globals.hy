(import
  curses)

(def globals-to-save (set))
(defmacro defs [varname value]
  ; Define a variable and mark it to be saved.
  (import [hy [HyString]])
  `(do
    (def ~varname ~value)
    (.add globals-to-save ~(HyString varname))))

;; * Parameters

(def debug False)
(def omnivision False)
(def always-passwall False)
(def super-speed False)

(def autosave True)

(def inventory-limit 8)
(def max-dungeon-level 19)

(def message-lines 5)
(def map-border-width 2)
(def text-screen-left-margin 1)
(def text-screen-page-overlap 2)

(def max-message-log-len 200)

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
(def unid-item-color :purple)
(def low-time-bg-color :red)
(def low-time-fg-color :white)
(def low-time-threshold 60) ; In seconds; converted to internal units below.

(def new-msg-highlight curses.A-BOLD)

(def key-escape "\x1b")

(def score-interval .8)

(defs invlets (list "abcdefghijklmnopqrstuvwxyz"))

(def color-numbers {
  :black 16
  :white 15
  :dark-gray 244
  :very-dark-gray 235
  :red 196
  :dark-red 88
  :pale-pink 219
  :green 46
  :dark-green 22
  :blue 21
  :dark-blue 18
  :pale-azure 117
  :purple 90
  :yellow 226
  :gold 178
  :orange 208
  :dark-orange 166
  :brown 94})

(def clock-factor 1000)
(def clock-unit-name "millisecond")
(def clock-unit-abbr "ms")

(def vision-radius 20)
(def repulsed-from-player-range 20)
(def speedup-soda-factor 10)
(def confusion-misdirect-prob .25)

;; * Declarations

(defs save-file-path None)
(defs scores-file-path None)
(defs dates {
  "bundle_created" None
  "started" None
  "saved" None
  "loaded" None})
(defs bundle-os None)
(defs bundle-git None)
(def version-info None)

(defs seeds None)
(defs map-rng-state None)

(def look-mode-legend-height 4)

(def bottom-border (max look-mode-legend-height (+ message-lines 1)))
  ; The extra 1 is for the status line.

(def T None) ; This will be set to a curses screen.
(def screen-width None)
(def screen-height None)
(def color-pairs {})
(def locale-encoding None)
(defs message-log []) ; List of (count, text) tuples.
(defs last-new-message-number -1)
(defs last-message-count 0)

(def screen-mode None)

(defs player None)

(def endgame False)
(defs dungeon-level None)
  ; 0-based, but displayed as 1-based.

(defs map-width 0)
(defs map-height 0)

; Times are in units of second / clock-factor.
(defs current-time 0)
(defs time-limit None)
(defs last-action-duration 0)
(*= low-time-threshold clock-factor)
(def super-low-time-threshold (* clock-factor 5))

(def fov-dirty? True)
(defs seen-map [])

(defs uniques-generated [])

(defs inventory [])
(def itypes {})
