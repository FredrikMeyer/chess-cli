(ns chess.core
  (:gen-class)
  (:require [clojure.string :as s]
            [chess.piece :refer [piece->unicode-string]]
            [chess.board :as board :refer [start-position]]
            [chess.moves :as moves :refer [move-piece]]
            [chess.utils :refer [number->letter]]
            ))

(defn print-board [board]
  (doseq [y (range 8 0 -1)]
    (doseq [x (range 1 9)]
      (let [letter (number->letter x)
            pos (keyword (str (name letter) y))
            piece (pos board)
            ]
        (if piece
          (do
            (print (piece->unicode-string piece)))
          (print " "))))
    (print "\u001b[0m")
    (println))
  (flush))

(defn input->move [inp]
  (let [[from to] (s/split inp #" ")]
    {
     :from (keyword from)
     :to (keyword to)}
    )
  )

(def game-state
  (atom {:board board/only-pawns ;start-position
         :turn :white}))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [move nil
         num-moves 0]
    (Thread/sleep 200)
    (when move
      (let [move-obj (input->move move)
            new-board (move-piece (:board @game-state) (:from move-obj) (:to move-obj))]
        (swap! game-state (fn [state]
                            (-> state
                                (assoc :board new-board)
                                (assoc :turn (if (= :white (:turn state)) :black :white)))))
        )
      )
    (println "\u2654")
    (print "\033[2J")
    (print "\033[2;1H")
    (println "Move:" move " Turn: " (:turn @game-state))
    (print-board (:board @game-state))
    (print "\r")
    ;; (pr x)
    (println)
    (println "Which move? (write [from] [to])")
    (when (< num-moves 5)
      (recur (read-line) (inc num-moves)))
    ))
