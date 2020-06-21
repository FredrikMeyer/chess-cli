(ns chess.utils
  (:require [clojure.string :as str]))

(defn letter->number [l]
  (case l
    :a 1
    :b 2
    :c 3
    :d 4
    :e 5
    :f 6
    :g 7
    :h 8
    nil))

(defn number->letter [n]
  (case n
    1 :a
    2 :b
    3 :c
    4 :d
    5 :e
    6 :f
    7 :g
    8 :h
    nil))

(defn square->coordinate-pair [square]
  (let [namee (name square)
        x (letter->number (keyword (str (first namee))))
        y (Integer/parseInt (apply str (rest namee)))
        ]
    (list x y)))

(defn coordinate-pair->square [[x y]]
  (keyword (str (name (number->letter x)) y)))

(defn coordinate-is-inside-board [[x y]]
  (if (or (nil? x) (nil? y)) false
        (and (<= x 8) (>= x 1) (<= y 8) (>= y 1))))

(defn is-inside-board [square]
  (let [[x y] (square->coordinate-pair square)]
    (coordinate-is-inside-board [x y])))
