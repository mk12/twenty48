;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.

(ns twenty48.grid-test
  (:require [clojure.test :refer :all]
            [twenty48.grid :refer :all]))

(def g0x0 (empty-grid 0))
(def g1x1 (empty-grid 1))
(def g2x2 (empty-grid 2))
(def g3x3 (empty-grid 3))

(def demo-grid
  {:side 3
   :score 0
   :cells [2 0 2
           8 4 4
           8 2 0]})

(deftest making-empty-grids
  (is (= g0x0 {:side 0 :score 0 :cells []}))
  (is (= g1x1 {:side 1 :score 0 :cells [0]}))
  (is (= g2x2 {:side 2 :score 0 :cells [0 0 0 0]}))
  (is (= g3x3 {:side 3 :score 0 :cells [0 0 0 0 0 0 0 0 0]})))

(deftest grid-coordinates
  (are [s i j ind]
       (= (coords->index (empty-grid s) i j) ind)
    0 0 0, 0
    1 0 0, 0
    7 0 0, 0
    2 1 1, 3
    7 1 1, 8
    7 6 6, 48
    4 2 3, 11))

(deftest grid-index-access
  (are [grid ind cell]
       (= (cell-at grid ind) cell)
    g1x1 0 0
    g3x3 8 0
    {:cells [1 2 42 7 9]} 2 42))

(deftest counting-cells
  (are [grid n]
       (= (n-cells grid) n)
    g0x0 0
    g1x1 1
    g2x2 4
    {:side 42} 1764))

(deftest detecting-full-grids
  (are [grid result]
       (= (boolean (grid-full? grid)) result)
    g0x0 true
    g1x1 false
    g3x3 false
    {:cells [1]} true
    {:cells [3 2 0]} false
    {:cells [7 7 9]} true))

(deftest finding-empty-cells
  (are [grid coll]
       (= (empty-cell-indices grid) coll)
    g0x0 []
    g1x1 [0]
    g2x2 [0 1 2 3]
    {:side 2 :cells [0 1 1 1]} [0]
    {:side 2 :cells [0 1 0 1]} [0 2]
    {:side 2 :cells [1 0 0 0]} [1 2 3]
    {:side 2 :cells [1 1 1 1]} []))

(deftest inserting-a-number
  (are [grid n result]
       (= (:cells (insert-one-num grid n)) result)
    g0x0 1 nil
    g0x0 7 nil
    g1x1 1 [1]
    g1x1 9 [9]
    {:side 2 :score 0 :cells [7 7 2 3]} 97 nil
    {:side 2 :score 0 :cells [3 3 2 0]} 42 [3 3 2 42]
    {:side 2 :score 0 :cells [9 0 2 9]} 43 [9 43 2 9])
  (is (some #{314} (-> 10 empty-grid (insert-one-num 314) :cells))))

(deftest inserting-numbers
  (are [grid nums result]
       (= (:cells (insert-numbers grid nums)) result)
    g0x0 [1] nil
    g0x0 [7] nil
    g1x1 [1] [1]
    g1x1 [1 1] nil
    g2x2 [2 2 2 2] [2 2 2 2]
    {:side 2 :score 0 :cells [2 0 0 1]} [9 9] [2 9 9 1]))

(deftest moving-grids
  (are [grid dir]
       (= (move-grid grid dir) grid)
    g0x0 :left
    g1x1 :down
    g2x2 :up
    g2x2 :right)
  (let [testcase
        [[2 0 2
          8 4 4
          8 2 0]
         :right
         [0 0 4
          8 4 4
          8 2 0]
         :up
         [8 4 4
          8 2 4
          0 0 0]
         :down
         [0 0 0
          0 4 0
          16 2 8]]
        expected (take-nth 2 testcase)
        directions (take-nth 2 (rest testcase))]
    (is (= expected
           (map :cells (reductions move-grid demo-grid directions))))))

; TODO
; seprarte out helpres
; test 2 2 4 4 -> 0 0 4 8
;      2 2 2 2 -> 0 0 4 4
;      etc

(deftest detecting-impasse
  (are [grid result] (= (boolean (impasse? grid)) result)
    nil true
    g0x0 true
    g1x1 false
    demo-grid false
    (insert-numbers demo-grid [1 1]) true))
