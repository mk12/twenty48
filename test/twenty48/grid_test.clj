;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.

(ns twenty48.grid-test
  (:require [clojure.test :refer :all]
            [twenty48.grid :refer :all]))

(deftest making-grids
  (are [n grid] (= (make-grid n) grid)
       0 {:side 0 :score 0 :cells []}
       1 {:side 1 :score 0 :cells [0]}
       2 {:side 2 :score 0 :cells [0 0 0 0]}))

(deftest grid-coordinates
  (are [s i j ind] (= (coords->index (make-grid s) i j) ind)
       0 0 0, 0
       1 0 0, 0
       7 0 0, 0
       2 1 1, 3
       7 1 1, 8
       7 6 6, 48
       4 2 3, 11))

(deftest grid-index-access
  (are [grid ind cell] (= (cell-at grid ind) cell)
       (make-grid 1) 0 0
       (make-grid 3) 8 0
       {:cells [1 2 42 7 9]} 2 42))

(deftest counting-cells
  (are [grid n] (= (n-cells grid) n)
       (make-grid 0) 0
       (make-grid 1) 1
       (make-grid 2) 4
       {:side 42} 1764))

(deftest detecting-full-grids
  (are [grid result] (= (boolean (grid-full? grid)) result)
       (make-grid 0) true
       (make-grid 1) false
       (make-grid 3) false
       {:cells [1]} true
       {:cells [3 2 0]} false
       {:cells [7 7 9]} true))

(deftest finding-empty-cells
  (are [grid coll] (= (empty-cell-indices grid) coll)
       (make-grid 0) []
       (make-grid 1) [0]
       (make-grid 2) [0 1 2 3]
       {:side 2 :cells [0 1 1 1]} [0]
       {:side 2 :cells [0 1 0 1]} [0 2]
       {:side 2 :cells [1 0 0 0]} [1 2 3]
       {:side 2 :cells [1 1 1 1]} []))

(deftest inserting-a-number
  (are [grid n result] (= (:cells (insert-number grid n)) result)
       (make-grid 0) 0 nil
       (make-grid 0) 7 nil
       (make-grid 1) 0 [0]
       (make-grid 1) 9 [9]
       {:side 2 :score 0 :cells [7 7 2 3]} 97 nil
       {:side 2 :score 0 :cells [3 3 2 0]} 42 [3 3 2 42]
       {:side 2 :score 0 :cells [9 0 2 9]} 43 [9 43 2 9])
  (is (some #{314} (-> 10 make-grid (rand-insert-number 314) :cells))))

(deftest inserting-numbers
  (are [grid nums result] (= (:cells (insert-numbers grid nums)) result)
       (make-grid 0) [0] nil
       (make-grid 0) [7] nil
       (make-grid 1) [1] [1]
       (make-grid 1) [1 1] nil
       (make-grid 2) [2 2 2 2] [2 2 2 2]))

(deftest moving-grids
  (is (= 1 1)))

(deftest detecting-impasse
  (are [grid result] (= (boolean (impasse? grid)) result)
       nil true))

