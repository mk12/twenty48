;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.

(ns twenty48.grid
  "Implements the 2048 grid model.")

(defn empty-grid
  "Returns a empty grid with side length n."
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

(defn insert-one-num
  "Inserts a number n in a random empty cell in the grid and returns the grid.
  If unsuccessful (no empty cells in the grid), returns nil."
  [grid n]
  {:pre [(pos? n)]}
  (if-let [indices (seq (empty-cell-indices grid))]
    (-> grid
        (assoc-in [:cells (rand-nth indices)] n)
        (update-in [:score] + n))))

(defn insert-numbers
  "Inserts a collection of numbers into empty cells in the grid by repeatedly
  calling insert-one-num. Returns nil if the length of the numbers collection is
  larger than the number of empty spaces in the grid."
  [grid numbers]
  (reduce
    (fn [g n]
      (if g
        (insert-one-num g n)
        (reduced nil)))
    grid
    numbers))

(defn move-grid
  "Slides the blocks in the grid in the direction dir, which must be one of
  :left :right :down :up. Colliding blocks containing the same number will merge
  to form a single block containing the sum. Returns the new grid."
  [grid dir]
  (letfn
   [(cells->rows
      [cells]
      nil)
    (rows->cells
      [rows]
      nil)
    (combine
      [a b]
      (condp = [a b]
        [a 0] [0 a]
        [a a] [0 (* 2 a)]
        [a b]))
    (shift-row
      [row]
      (let [[result last]
            (reduce
             (fn [[new-row a] b]
               (let [[a' b'] (combine a b)]
                 [(conj new-row a') b']))
             [[] 0]
             row)]
        (conj (subvec result 1) last)))]
    (update grid :cells
            #(->> % cells->rows (map shift-row) rows->cells))))

(defn impasse?
  "Returns true if the grid is full and no moves are possible. Also returns true
  when grid is nil because no move can be made on a nil grid."
  [grid]
  (or (not grid)
      (and (grid-full? grid)
           (every? #(= grid (move-grid grid %))
                     [:left :right :down :up]))))
