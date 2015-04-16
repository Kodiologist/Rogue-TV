(import
  curses
  [libtcodpy :as tcod])

;; * Parameters

(def map-width 80)
(def map-height 40)

(def inventory-limit 10)

(def message-lines 3)

(def fg-color :black)
(def bg-color :white)
(def unseen-color :dark-gray)
(def announcer-colors {
  :tara :blue
  :bob :red})

(def new-msg-highlight curses.A-BOLD)

(def key-escape "\x1b")

(def invlets (list "abcdefghijklmnopqrstuvwxyz"))

(def color-numbers {
  :black 16
  :white 15
  :light-gray 7
  :dark-gray 8
  :green 2
  :blue 12
  :red 9
  :yellow 3})

;; * Declarations

(def bottom-border (+ message-lines 1))
  ; The extra 1 is for the status line.

(def T None) ; This will be set to a curses screen.
(def screen-width None)
(def screen-height None)
(def color-pairs {})
(def message-log [])
(def last-new-message-number -1)

(def player None)

(def endgame False)
(def dungeon-level None)

; Times are in simulated seconds.
(def current-time 0)
(def time-limit None)
(def last-action-duration 0)

(def fov-map (tcod.map-new map-width map-height))
(def seen-map [])

(def inventory [])
(def itypes {})
