;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.

(ns twenty48.grid-test
  (:require [clojure.test :refer :all]
            [twenty48.grid :refer :all]))

(def g0x0 (empty-grid 0))
(def g1x1 (empty-grid 1))
(def g2x2 (empty-grid 2))
(def g3x3 (empty-grid 3))

(def demo-grid
  (grid-from-2d-vec
   [[2 0 2]
    [8 4 4]
    [8 2 0]]))

(deftest making-empty-grids
  (is (= g0x0 {:side 0 :score 0 :counter 0 :blocks []}))
  (is (= g1x1 {:side 1 :score 0 :counter 0 :blocks []}))
  (is (= g2x2 {:side 2 :score 0 :counter 0 :blocks []})))

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
    {:side 1 :blocks [1]} true
    {:side 2 :blocks [1 1 1]} false
    {:side 2 :blocks [1 1 1 1]} true))

(deftest finding-empty-cells
  (are [grid coord-set]
       (= (set (empty-cell-coords grid)) coord-set)
    g0x0 #{}
    g1x1 #{[0 0]}
    g2x2 #{[0 0] [0 1] [1 0] [1 1]}
    demo-grid #{[1 0] [2 2]}))

(deftest inserting-a-block
  (are [grid n result]
       (= (:blocks (insert-one-block grid n)) result)
    g0x0 1 nil
    g1x1 9 [{:id 0 :value 9 :pos [0 0]}])
  (is (some #{42}
            (-> (empty-grid 5)
                (insert-one-block 42)
                :blocks
                (->> (map :value))))))

(deftest inserting-blocks
  (are [grid nums result]
       (= (->> (insert-blocks grid nums) :blocks (map :value) sort seq)
          result)
    g0x0 [1] nil
    g1x1 [1] [1]
    g1x1 [1 1] nil
    g2x2 [1 4 3 2] [1 2 3 4]
    demo-grid [100 101] [2 2 2 4 4 8 8 100 101]))

; (deftest moving-grids
;   (are [grid dir]
;        (= (move-grid grid dir) grid)
;     g0x0 :left
;     g1x1 :down
;     g2x2 :up
;     g2x2 :right)
;   (let [testcase
;         [[2 0 2
;           8 4 4
;           8 2 0]
;          :right
;          [0 0 4
;           8 4 4
;           8 2 0]
;          :up
;          [8 4 4
;           8 2 4
;           0 0 0]
;          :down
;          [0 0 0
;           0 4 0
;           16 2 8]]
;         expected (take-nth 2 testcase)
;         directions (take-nth 2 (rest testcase))]
;     (is (= expected
;            (map :cells (reductions move-grid demo-grid directions))))))

; TODO
; seprarte out helpres
; test 2 2 4 4 -> 0 0 4 8
;      2 2 2 2 -> 0 0 4 4
;      etc

; (deftest detecting-impasse
;   (are [grid result] (= (boolean (impasse? grid)) result)
;     nil true
;     g0x0 true
;     g1x1 false
;     demo-grid false
;     (insert-numbers demo-grid [1 1]) true))
