(ns chess.utils-test
  (:require [chess.utils :as sut]
            [clojure.test :refer :all]))

(deftest letter-number-inverse
  (doseq [x (range 1 9)]
    (is (= x ((comp sut/letter->number
                    sut/number->letter) x)))))


