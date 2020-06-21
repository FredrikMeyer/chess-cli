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

(defn get-rank
  "Return all squares at a given rank (row)."
  [row]
  (for [y (range 1 9)]
    (let [current-letter (name (number->letter y))]
      (keyword (str current-letter row)))))

(defn get-file
  "Get all squares in a given file (column)."
  [file]
  (for [x (range 1 9)]
    (keyword (str (name file) x))))

(defn get-north-east-diagonal
  "Get diagonals centered at position."
  [at]
  (let [[x y] (square->coordinate-pair at)
        [lx ly] (if (>= y x) [1 (+ (- y x) 1)] [(+ (- x y) 1) 1])]
    (->> (for [xx (range 8)]
          [(+ lx xx) (+ ly xx)])
         (filter coordinate-is-inside-board)
         (map coordinate-pair->square))))

(defn get-south-east-diagonal
  "Get south east diagonals, including arg."
  [at]
  (let [[x y] (square->coordinate-pair at)
        [lx ly] (if (<= x (- 9 y)) [1 (- (+ y x) 1)] [(+ (- y x) 1) 8])]
    (->> (for [xx (range 8)]
          [(+ lx xx) (- ly xx)])
         (filter coordinate-is-inside-board)
         (map coordinate-pair->square))))

(defn get-diagonals
  [at]
  (-> (concat (get-north-east-diagonal at) (get-south-east-diagonal at))
      (distinct)))
