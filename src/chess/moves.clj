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

(defn moves-for-pawn [board-state position]
  (let [board (:board board-state)
        piece (position board)
        at-start? (start-position-for-pawn? position (:color piece))
        ]
    (if at-start?
      [(move-forward board-state position) (move-two-forward board-state position)]
      [(move-forward board-state position)]
      )))

(defn moves-for-knight [_ position]
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

(defn moves-for-rook [_ position]
  (let [[x y] (u/square->coordinate-pair position)
        row (-> x (u/number->letter) (u/get-file))
        file (u/get-rank y)]
    (->> (concat row file)
        (distinct)
        (filter (fn [sq] (not= sq position)))
        (map (fn [sq] {
                       :from position
                       :to sq
                       })))))

(defn moves-for-bishop [_ position]
  (let [diagonals (u/get-diagonals position)]
    (->> diagonals
         (filter (fn [sq] (not= sq position)))
         (map (fn [sq] {
                        :from position
                        :to sq
                        })))))

(defn moves-for-king [_ position]
  (let [[x y] (u/square->coordinate-pair position)]
    (->> (for [dx [-1 0 1] dy [-1 0 1]]
           [(+ x dx) (+ y dy)])
         (filter u/coordinate-is-inside-board)
         (map u/coordinate-pair->square)
         (filter (fn [sq] (not= sq position)))
         (map (fn [sq] {
                        :from position
                        :to sq
                        })))))

(defn moves-for-queen [board-state position]
  (concat (moves-for-bishop board-state position)
          (moves-for-rook board-state position)))

(defn moves-for-piece [board-state position]
  (let [board (:board board-state)
        piece (position board)
        type (:type piece)]
    (case type
      :pawn (moves-for-pawn board-state position)
      :rook (moves-for-rook board-state position)
      :knight (moves-for-rook board-state position)
      :bishop (moves-for-bishop board-state position)
      :king (moves-for-king board-state position)
      :queen (moves-for-queen board-state position)
      nil (throw (AssertionError. (str "No piece at position: " position)))
      (throw (AssertionError. (str "Unknown piece type: " type))))))


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
    (concat one-step-moves two-step-moves)))
