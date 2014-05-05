;;; Copyright 2013 Mitchell Kember. Subject to the MIT License.

(ns twenty48.grid
  "Implements the 2048 grid model.")

(defn make-grid
  "Returns a grid with side length n filled with zeros."
  [n]
  {:side n :score 0 :cells (vec (repeat (* n n) 0))})

(defn coords->index
  "Converts a pair of coordinates to an index into the grid's cells."
  [grid i j]
  (+ (* i (:side grid)) j))

(defn cell-at
  "Returns the contents of the cell with the given index in the grid."
  [grid ind]
  ((:cells grid) ind))

(defn n-cells
  "Returns the number of cells in the grid."
  [grid]
  (let [side (:side grid)]
    (* side side)))

(defn grid-full?
  "Returns true if all cells in the grid are nonzero, false otherwise."
  [grid]
  (not-any? zero? (:cells grid)))

(defn empty-cell-indices
  "Returns a lazy sequence of the indices of all empty cells in the grid."
  [grid]
  (filter #(zero? (cell-at grid %))
          (range (n-cells grid))))

(defn insert-number
  "Inserts a number n in a random empty cell in the grid and returns the grid.
  If unsuccessful (no empty cells in the grid), returns nil."
  [grid n]
  (let [inds (empty-cell-indices grid)]
    (if (seq inds)
      (-> grid
          (assoc-in [:cells (rand-nth inds)] n)
          (update-in [:score] + n)))))

(defn insert-numbers
  "Like insert-number, but inserts a collection of numbers."
  [grid numbers]
  (reduce
    (fn [g n]
      (if g
        (insert-number g n)
        (reduced nil)))
    grid
    numbers))

(defn move-grid
  "Slides the blocks in the grid in the direction dir, which must be one of
  :left :right :down :up. Colliding blocks containg the same number will merge
  to form a single block containing the sum. Returns the new grid."
  [grid dir]
  grid)

(defn impasse?
  "Returns true if the grid is full and no moves are possible. Also returns true
  when grid is nil."
  [grid]
  (or (not grid)
      (and (grid-full? grid)
           (every? #(= grid (move-grid grid %))
                     [:left :right :down :up]))))
