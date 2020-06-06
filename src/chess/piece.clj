(ns chess.piece
  (:require [clojure.string :as s]))

(defn piece [color type]
  {
   :type type
   :color color
   })

(defn piece->unicode-string [p]
  (case (:color p)
    :white (case (:type p)
             :rook "\u2656"
             :knight "\u2658"
             :bishop "\u2657"
             :king "\u2654"
             :queen "\u2655"
             :pawn \u2659)
    :black (case (:type p)
             :rook "\u265C"
             :knight "\u265E"
             :bishop "\u265D"
             :king "\u265A"
             :queen "\u265B"
             :pawn "\u265F")
    ))


;; Below functions are unused

(defn type->string [t]
  (case t
    :rook "r"
    :knight "n"
    :bishop "b"
    :queen "q"
    :king "k"
    :pawn "p"
    nil nil))

(defn piece->string [p]
  (let [{color :color type :type} p
        ps (type->string type)]
    (comment 
      (if (= color :white)
        (do
          ;; (print "\u001b[30;1m")
          ;; (print "\u001b[47m")
          (s/upper-case ps))
        (do
          ;; (print "\u001b[37;1m")
          ;; (print "\u001b[40m")
          ps)))
    (piece->unicode-string p)))

(defn color->string [c]
  (case c
    :white "w"
    "b"))

(defn string->type [t]
  (case (s/lower-case t)
    "r" :rook
    "n" :knight
    "b" :bishop
    "q" :queen
    "k" :king
    :pawn
    ))

(defn string->piece [t]
  (let [[c p] t
        type (string->type p)]
    (case c
      \w (piece :white type)
      \b (piece :black type))))
