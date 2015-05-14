(import roguetv.main)

(roguetv.main.new-game)

(try (import roguetv-init)
  (catch [_ ImportError]))

(roguetv.main.main-loop)
