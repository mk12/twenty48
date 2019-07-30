;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.

(ns twenty48.grid-test
  (:require [clojure.test :refer :all]
            [twenty48.grid :refer :all]))

(def g0x0 (empty-grid 0))
(def g1x1 (empty-grid 1))
(def g2x2 (empty-grid 2))
(def g3x3 (empty-grid 3))

(def demo-grid
  (matrix->grid
   [[2 0 2]
    [8 4 4]
    [8 2 0]]))

(deftest making-empty-grids
  (is (= g0x0 {:side 0 :score 0 :counter 0 :blocks []}))
  (is (= g1x1 {:side 1 :score 0 :counter 0 :blocks []}))
  (is (= g2x2 {:side 2 :score 0 :counter 0 :blocks []})))

(deftest grid-matrix-conversion
  (are [grid matrix]
       (= (grid->matrix grid) matrix)
    g0x0 []
    g1x1 [[0]]
    g2x2 [[0 0]
          [0 0]])
  (are [matrix]
       (= matrix (grid->matrix (matrix->grid matrix)))
    []
    [[1]]
    [[1 0 3]
     [0 2 2]
     [4 4 8]]))

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

(deftest inserting-one-block
  (are [grid n result]
       (= (:blocks (insert-one-block grid n)) result)
    g0x0 1 nil
    g1x1 9 [{:id 0 :value 9 :pos [0 0]}])
  (is (some #{42}
            (-> (empty-grid 5)
                (insert-one-block 42)
                :blocks
                (->> (map :value))))))

(deftest inserting-multiple-blocks
  (are [grid nums result]
       (= (->> (insert-blocks grid nums) :blocks (map :value) sort seq)
          result)
    g0x0 [1] nil
    g1x1 [1] [1]
    g1x1 [1 1] nil
    g2x2 [1 4 3 2] [1 2 3 4]
    demo-grid [100 101] [2 2 2 4 4 8 8 100 101]))

(deftest sliding-grids
  (are [grid dir]
       (let [[new-grid combined] (slide-grid grid dir)]
         (and (= grid new-grid) (empty? combined)))
    g0x0 :left
    g1x1 :down
    g2x2 :up
    g3x3 :right)
  (are [dir combined-values]
       (= combined-values
          (sort (map :value ((slide-grid demo-grid dir) 1))))
    :right [2 2 4 4]
    :left [2 2 4 4]
    :up [8 8]
    :down [8 8])
  (let [testcase
        [[[2 0 2]
          [8 4 4]
          [8 2 0]]
         :right
         [[0 0 4]
          [0 8 8]
          [0 8 2]]
         :up
         [[0 16 4]
          [0 0 8]
          [0 0 2]]
         :down
         [[0 0 4]
          [0 0 8]
          [0 16 2]]]
        expected (take-nth 2 testcase)
        directions (take-nth 2 (rest testcase))
        slide (fn [grid dir] ((slide-grid grid dir) 0))]
    (is (= expected
           (map grid->matrix
                (reductions slide demo-grid directions))))))

(deftest detecting-available-directions
  (are [grid dirs]
       (= dirs (set (available-slide-directions grid)))
    g0x0 #{}
    g1x1 #{}
    demo-grid #{:left :right :up :down}
    (matrix->grid
     [[2 4 2]
      [4 2 4]
      [2 0 2]]) #{:left :right :down}
    (matrix->grid
     [[2 4 2]
      [4 2 4]
      [2 4 2]]) #{}))
