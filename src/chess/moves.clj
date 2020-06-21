(ns chess.moves
  (:require [chess.utils :as u :refer [letter->number]]))

(defn move-piece [board from to]
  (let [piece (from board)
        board-without-piece (dissoc board from)]
    (assoc board-without-piece (keyword to) piece)))


(defn move-forward
  "Should return a move. Sign (board-state, square) -> move
  We want board-state -> (move -> move)
  "
  [board-state square]
  (let [color (:turn board-state)
        ;; board (:board board-state)
        inc-function (if (= color :white) inc dec)
        [x y] (u/square->coordinate-pair square)
        new-coords [x (inc-function y)]
        new-letter-coord (u/coordinate-pair->square new-coords)
        ]
    {
     :from square
     :to new-letter-coord
     }
    ))

(defn move-two-forward
  [board-state square]
    (let [color (:turn board-state)
          inc-function (if (= color :white) (comp inc inc) (comp dec dec))
          [x y] (u/square->coordinate-pair square)
          new-coords [x (inc-function y)]
          new-letter-coord (u/coordinate-pair->square new-coords)
        ]
    {
     :from square
     :to new-letter-coord
     }
    )
  )


(defn start-position-for-pawn? [position color]
  (let [[_ y] (u/square->coordinate-pair position)]
    (if (= color :white)
      (= y 2)
      (= y 7))))

;; board-state, position -> list[moves] ?
;; dispatch på piece-type

(defn moves-for-pawn [board-state position]
  (let [board (:board board-state)
        piece (position board)
        at-start? (start-position-for-pawn? position (:color piece))
        ]
    (if at-start?
      [(move-forward board-state position) (move-two-forward board-state position)]
      [(move-forward board-state position)]
      )))

(defn moves-for-knight [board-state position]
  (let [[x y] (u/square->coordinate-pair position)
        knight-moves (mapcat identity (for [a [-2 2] b [-1 1]] [[a b] [b a]]))
        raw-moves (map (fn [[a b]] [(+ a x) (+ b y)]) knight-moves)]
    (->> raw-moves
        (filter u/coordinate-is-inside-board)
        (map u/coordinate-pair->square)
        (map (fn [sq] {
                       :from position
                       :to sq
                       })))))


(defn pawn-moves [board-state]
  (let [board (:board board-state)
        pawns (select-keys board
                           (for [[k v] board :when (and
                                                    (= (:color v) :white)
                                                    (= (:type v) :pawn))] k))
        one-step-moves (for [[k _] pawns]
                         (move-forward board-state k))
        two-step-moves (for [[k _] pawns]
                         (move-two-forward board-state k))]
    (concat one-step-moves two-step-moves)
    )
  )



(defn test-f []
  (fn [] 2))
