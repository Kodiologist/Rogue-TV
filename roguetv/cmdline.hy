(require kodhy.macros)

(import
  random
  argparse
  [kodhy.util [keyword->str str->keyword]]
  [roguetv.english [genders NounPhrase]])

(def parameters [
  ["name"
    :help "name of your character"]
  ["gender"
    :help "gender of your character"
    :choices (amap (keyword->str it) genders)]
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

  (unless p.gender
    (setv p.gender (random.choice ["male" "female"])))
  (setv p.gender (str->keyword p.gender))

  (unless p.name
    (setv p.name (cond
      [(= p.gender :male)
        (random.choice (qw Matthew Mark Luke John))]
          ; The four gospels
      [(= p.gender :female)
        (random.choice (qw Meg Jo Beth Amy))]
          ; Little Women
      [(= p.gender :neuter)
        (random.choice (qw Zorx Klax Jennifer))]
          ; Captain Underpants and the Invasion of the Incredibly Naughty Cafeteria Ladies from Outer Space (and the Subsequent Assault of the Equally Evil Lunchroom Zombie Nerds)
      [True
        (random.choice (qw Mac Nancy))])))
          ; Wayside School
  (setv p.name (kwc NounPhrase p.name :+proper))

  p)
