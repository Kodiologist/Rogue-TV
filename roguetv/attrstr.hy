(require [roguetv.macros [*]])

(import
  re
  xml.etree.ElementTree
  curses
  [kodhy.util [str->keyword keyword->str]]
  [roguetv.globals :as G]
  roguetv.xterm-colors)

(defn get-color [fg &optional bg]
  (when (none? fg)
    (setv fg G.fg-color))
  (when (none? bg)
    (setv bg (G.pick-bg-color fg)))
  (curses.color-pair (try
    (get G.color-pairs (, fg bg))
    (except [_ KeyError]
      ; This color pair hasn't been initialized yet. So do that.
      ; First ensure each color is defined correctly to have its
      ; usual value under xterm.
      (when (curses.can-change-color)
        (for [c [fg bg]]
          (setv cn (get G.color-numbers c))
          (apply curses.init-color (+ [cn] (amap
            (int (* it (/ 1000 256)))
            (get roguetv.xterm-colors.table cn))))))
      ; Now create the color pair.
      (setv i (+ 2 (len G.color-pairs)))
      (curses.init-pair i (get G.color-numbers fg) (get G.color-numbers bg))
      (setv (get G.color-pairs (, fg bg)) i)
      i))))

(defn default-color []
  (get-color G.fg-color))

(defn curses-encode [s]
  (.encode s G.locale-encoding))

(defn -elem-attrs [e]
  ; Translates an xml.etree.ElementTree.Element to curses
  ; attributes.
  (ecase e.tag
    ["root"
      0]
    ["b"
      curses.A-BOLD]
    ["c"
      (get-color
        (and (.get e "fg") (str->keyword (.get e "fg")))
        (and (.get e "bg") (str->keyword (.get e "bg"))))]))

(defn -from-xml-f [elem a]
  (|= a (-elem-attrs elem))
  (setv chars (or elem.text ""))
  (setv attrs (* [a] (len chars)))
  (for [child elem]
    (setv [new-chars new-attrs] (-from-xml-f child a))
    (+= chars (+ new-chars (or child.tail "")))
    (+= attrs (+ new-attrs (* [a] (len (or child.tail ""))))))
  (, chars attrs))

(setv ws-re (re.compile r"(\s+)"))

(defclass AttrStr [object] [
; An AttrStr is a string with an accompanying list of curses
; attributes for each character.

  __init__ (fn [self &optional chars attrs]
    (setv self.chars (or chars ""))
    (setv self.attrs (or attrs []))
    (assert (= (len self.chars) (len self.attrs)))
    None)

  from-xml (classmethod (fn [self xml-string]
    (setv root (xml.etree.ElementTree.fromstring
      (.encode (+ "<root>" xml-string "</root>") "UTF-8")))
    (apply AttrStr (-from-xml-f root 0))))

  __repr__ (fn [self]
    (.format "AttrStr({!r}, {!r})" self.chars self.attrs))

  __len__ (fn [self]
    (len self.chars))

  draw (fn [self &optional extra-attrs]
    (for [[c a] (zip self.chars self.attrs)]
      (try
        (G.T.addstr (curses-encode c) (| (or a 0) (or extra-attrs 0)))
        (except [_ curses.error] None))))
          ; http://bugs.python.org/issue8243

  ljust (fn [self width]
    (AttrStr
      (.ljust self.chars width)
      (+ self.attrs (* [None] (max 0 (- width (len self.chars)))))))

  trunc (fn [self width]
    (if (<= (len self) width)
      self
      (AttrStr (cut self.chars 0 width) (cut self.attrs 0 width))))

  wrap (fn [self width]
    ; Returns a list of AttrStrs, each no longer than the
    ; specified width.
    (setv lines [])
    (setv line-chars "")
    (setv line-attrs [])
    (setv chunks (.split ws-re self.chars))
    (setv attrs (list self.attrs))
    (defn shift-attrs [n]
      ; Removes and returns the first `n` attrs.
      (amap (.pop attrs 0) (range n)))
    ; Remove trailing whitespace.
    (when (.isspace (get chunks -1))
      (.pop chunks))
    ; Begin the list with a zero-length fake whitespace
    ; element.
    (when (.isspace (first chunks))
      (shift-attrs (len (first chunks)))
      (del (get chunks 0)))
    (.insert chunks 0 "")
    (for [[ws nonws] (zip (cut chunks 0 None 2) (cut chunks 1 None 2))]
      (when (> (+ (len line-chars) (len ws) (len nonws)) width)
        ; This line is full. Move on to the next line.
        (.append lines (AttrStr line-chars line-attrs))
        (setv line-chars "")
        (setv line-attrs [])
        ; Clear the whitespace chunk. We don't need whitespace
        ; at the beginning of a line.
        (shift-attrs (len ws))
        (setv ws ""))
      ; Append the chunks.
      (+= line-chars (+ ws nonws))
      (+= line-attrs (shift-attrs (+ (len ws) (len nonws)))))
    (.append lines (AttrStr line-chars line-attrs))
    lines)])
