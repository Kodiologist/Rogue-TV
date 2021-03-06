(require [kodhy.macros [amap amap2 whenn λ qw]])

(import
  sys
  os
  os.path
  errno
  random
  [collections [OrderedDict]]
  argparse
  appdirs
  [kodhy.util [T F ret by-ns concat keyword->str str->keyword]]
  [roguetv.english [genders NounPhrase]]
  [roguetv.globals :as G])

(defn parse-env []
  (setv G.version-info "Not a bundled version")
  (whenn (os.getenv "ROGUETV_BUNDLE_INFO")
    (setv [l1 l2 l3] (.split it "\n"))
    (setv G.bundle-os l1)
    (setv G.bundle-git (cut l2 (len "Git commit ")))
    (setv (get G.dates "bundle_created") (cut l3 (len "Packaged at ")))
    (setv G.version-info (.format "Bundle version {}-{} ({})"
      (cut G.bundle-git 0 12) G.bundle-os (get G.dates "bundle_created")))))

(def pronouns->genders (OrderedDict [
  (, "he" :male)
  (, "she" :female)
  (, "it" :neuter)]))

(defn uni [s]
  (.decode s (sys.getfilesystemencoding)))

(defn parse-args [&optional args]
  (setv desc (+ "Rogue TV by Kodi Arfer\n" G.version-info))

  (setv parser (argparse.ArgumentParser
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
    ["map-seed" :type int
      :metavar "INTEGER"
      :help "RNG seed for generating the dungeon (new game only)"]
    ["general-seed" :type int
      :metavar "INTEGER"
      :help "RNG seed for all other events (new game only)"]
    ["start-at-dl"
      :metavar "INTEGER"
      :type (λ
        (setv it (- (int it) 1))
        (unless (<= 0 it G.max-dungeon-level)
          (raise (argparse.ArgumentTypeError "no such dungeon level")))
        it)
      :help "starting dungeon level (new game only)"]
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
    (.add-argument parser (+ "--" (first x)) #** (dict
      (amap2 (, (keyword->str a) b) (rest x)))))
  (setv p (.parse-args parser args))

  (unless p.save
    (setv p.save (os.path.join (default-dir) "saved-game.json.gz")))

  (unless p.scores
    (setv p.scores (os.path.join (default-dir) "scores.json")))

  (for [a ["map_seed" "general_seed"]]
    (unless (getattr p a)
      (setattr p a (random.randrange (- (** 2 63)) (- (** 2 63) 1)))))

  (unless p.pronouns
    (when p.name
      (sys.exit "You set --name, so you probably want to set --pronouns, too."))
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
      [T
        (random.choice (+
           (qw Zorx Klax Jennifer)
             ; Captain Underpants and the Invasion of the Incredibly Naughty Cafeteria Ladies from Outer Space (and the Subsequent Assault of the Equally Evil Lunchroom Zombie Nerds)
           ["Robert'); DROP TABLE Players;--"]))])))
            ; http://www.xkcd.com/327/
  (setv p.name (NounPhrase p.name :bare-proper T :gender p.gender))

  p)

(defn default-dir []
  (if (= G.bundle-os "windows")
    ".." ; This should be the bundle directory.
    (do
      (setv d (appdirs.user-data-dir "Rogue TV" "Kodiologist"))
      (try
        (os.makedirs d)
        (except [e OSError]
          (unless (= e.errno errno.EEXIST)
            (raise))))
      d)))
