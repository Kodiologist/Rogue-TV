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
  [roguetv.english [genders NounPhrase]])

(setv pronouns->genders (OrderedDict [
  (, "he" :male)
  (, "she" :female)
  (, "it" :neuter)
  (, "they" :singular-they)]))

(defn uni [s]
  (.decode s (sys.getfilesystemencoding)))

(def parameters [
  ["name" :type uni
    :help "name of your character (new game only)"]
  ["pronouns" :type uni
    :help "pronouns for your character (new game only)"
    :choices (amap (str it) (.keys pronouns->genders))]
  ["save" :type uni
    :help "filepath to read a saved game from or write saved games to"]
  ["debug"
    :help "enable debug mode"
    :action "store_true"]])

(defn parse-args [&optional args]
  (setv parser (argparse.ArgumentParser))
  (for [x parameters]
    (apply .add-argument
      [parser (+ "--" (first x))]
      (dict (amap2 (, (keyword->str a) b) (rest x)))))
  (setv p (.parse-args parser args))

  (unless p.save
    (setv d (appdirs.user-data-dir "Rogue TV" "Kodiologist"))
    (try
      (os.makedirs d)
      (catch [e OSError]
        (unless (= e.errno errno.EEXIST)
          (raise))))
    (setv p.save (os.path.join d "saved-game.json.gz")))

  (unless p.pronouns
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
      [(= p.gender :neuter)
        (random.choice (+
           (qw Zorx Klax Jennifer)
             ; Captain Underpants and the Invasion of the Incredibly Naughty Cafeteria Ladies from Outer Space (and the Subsequent Assault of the Equally Evil Lunchroom Zombie Nerds)
           ["Robert'); DROP TABLE Players;--"]))]
            ; http://www.xkcd.com/327/
      [True
        (random.choice (qw Mac Nancy))])))
          ; Wayside School
  (setv p.name (kwc NounPhrase p.name :+bare-proper :gender p.gender))

  p)
