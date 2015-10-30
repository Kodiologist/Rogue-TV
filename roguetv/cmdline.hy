(require kodhy.macros)

(import
  sys
  os
  os.path
  errno
  random
  [collections [OrderedDict]]
  argparse
  appdirs
  [kodhy.util [keyword->str str->keyword]]
  [roguetv.english [genders NounPhrase]]
  [roguetv.globals :as G])

(defn parse-env []
  (setv G.version-info "Not a bundled version")
  (whenn (os.getenv "ROGUETV_BUNDLE_INFO")
    (setv [l1 l2 l3] (.split it "\n"))
    (setv G.bundle-os l1)
    (setv G.bundle-git (slice l2 (len "Git commit ")))
    (setv (get G.dates "bundle_created") (slice l3 (len "Packaged at ")))
    (setv G.version-info (.format "Bundle version {}-{} ({})"
      (slice G.bundle-git 0 12) G.bundle-os (get G.dates "bundle_created")))))

(def pronouns->genders (OrderedDict [
  (, "he" :male)
  (, "she" :female)
  (, "it" :neuter)]))

(defn uni [s]
  (.decode s (sys.getfilesystemencoding)))

(defn parse-args [&optional args]
  (setv desc (+ "Rogue TV by Kodi Arfer\n" G.version-info))

  (setv parser (kwc argparse.ArgumentParser
    :formatter-class argparse.RawDescriptionHelpFormatter
    :description desc))

  (setv parameters [
    ["version"
      :action "version"
      :version desc]
    ["name" :type uni
      :metavar "TEXT"
      :help "name of your character (new game only)"]
    ["pronouns" :type uni
      :help "pronouns for your character (new game only)"
      :choices (amap (str it) (.keys pronouns->genders))]
    ["save" :type uni
      :metavar "FILEPATH"
      :help "where to read saved games and write saved games to"]
    ["no-autosave"
      :help "don't automatically save at the end of each level"
      :action "store_true"]
    ["scores" :type uni
      :metavar "FILEPATH"
      :help "where to store scores"]
    ["show-scores"
      :help "instead of starting a game, show scores"
      :action "store_true"]
    ["show-all-scores"
      :help "like --show-scores, but show every game"
      :action "store_true"]
    ["debug"
      :help "enable debug mode"
      :action "store_true"]])

  (for [x parameters]
    (apply .add-argument
      [parser (+ "--" (first x))]
      (dict (amap2 (, (keyword->str a) b) (rest x)))))
  (setv p (.parse-args parser args))

  (unless p.save
    (setv p.save (os.path.join (default-dir) "saved-game.json.gz")))

  (unless p.scores
    (setv p.scores (os.path.join (default-dir) "scores.json")))

  (unless p.pronouns
    (when p.name
      (print "You set --name, so you probably want to set --pronouns, too.")
      (exit))
    (setv p.pronouns (random.choice ["he" "she"])))
  (setv p.gender (get pronouns->genders p.pronouns))

  (unless p.name
    (setv p.name (cond
      [(= p.gender :male)
        (random.choice (qw Matthew Mark Luke John))]
          ; The four gospels
      [(= p.gender :female)
        (random.choice (qw Meg Jo Beth Amy))]
          ; Little Women
      [True
        (random.choice (+
           (qw Zorx Klax Jennifer)
             ; Captain Underpants and the Invasion of the Incredibly Naughty Cafeteria Ladies from Outer Space (and the Subsequent Assault of the Equally Evil Lunchroom Zombie Nerds)
           ["Robert'); DROP TABLE Players;--"]))])))
            ; http://www.xkcd.com/327/
  (setv p.name (kwc NounPhrase p.name :+bare-proper :gender p.gender))

  p)

(defn default-dir []
  (if (= G.bundle-os "windows")
    ".." ; This should be the bundle directory.
    (do
      (setv d (appdirs.user-data-dir "Rogue TV" "Kodiologist"))
      (try
        (os.makedirs d)
        (catch [e OSError]
          (unless (= e.errno errno.EEXIST)
            (raise))))
      d)))
