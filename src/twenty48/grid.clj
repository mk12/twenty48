;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.

(ns twenty48.grid
  "Implements the 2048 grid model.")

(defn empty-grid
  "Returns a empty grid with side length n."
  [n]
  {:side n      ; side length
   :score 0     ; sum of merged block values
   :counter 0   ; counter for block IDs
   :blocks []}) ; active blocks

(defn n-cells
  "Returns the number of cells in the grid."
  [grid]
  (let [s (:side grid)] (* s s)))

(defn grid-full?
  "Returns true if all cells in the grid are filled, false otherwise."
  [grid]
  (= (count (:blocks grid)) (n-cells grid)))

(defn empty-cell-coords
  "Returns the coordinates of empty cells in the grid."
  [grid]
  (let [taken (set (map :pos (:blocks grid)))
        indices (range (:side grid))
        all (for [x indices, y indices] [x y])]
    (filter (complement taken) all)))

(defn insert-one-block
  "Inserts a number n in a random empty cell in the grid and returns the grid.
  If unsuccessful (no empty cells in the grid), returns nil."
  [grid n]
  {:pre [(pos? n)]}
  (if-let [pos (rand-nth (seq (empty-cell-coords grid)))]
    (-> grid
        (update :counter inc)
        (update :blocks conj {:id (:counter grid) :value n :pos pos}))))

(defn insert-blocks
  "Inserts a collection of numbers into empty cells in the grid by repeatedly
  calling insert-one-block. Returns nil if the length of the numbers collection
  is larger than the number of empty spaces in the grid."
  [grid numbers]
  (reduce
    (fn [g n] (if g (insert-one-block g n) (reduced nil)))
    grid
    numbers))

(defn opposite-direction
  "Returns the direction opposite dir."
  [dir]
  ({:left :right, :right :left, :up :down, :down :up} dir))

(defn arrange-blocks
  "Returns the grid's blocks arranged in rows for the direction dir, which must
  be one of :left :right :down :up."
  [grid dir]
  (let [blocks (:blocks grid)
        pos-map (zipmap (map :pos blocks) blocks)
        side (:side grid)
        forward (range side)
        backward (reverse forward)
        directed (if (#{:down :right} dir) forward backward)
        horizontal (#{:left :right} dir)
        coords (for [a forward]
                 (for [b directed]
                   (if horizontal [b a] [a b])))]
    (map #(remove nil? (map pos-map %))
         coords)))

(defn slide-block-to-edge
  "Slides a block to the edge of the grid."
  [block dir grid]
  (let [[x y] (:pos block)
        edge (dec (:side grid))
        new-pos (case dir
                  :left [0 y]
                  :right [edge y]
                  :up [x 0]
                  :down [x edge])]
    (assoc block :pos new-pos)))

(defn slide-block-against
  "Slides a block to be adjacent to another block."
  [block dir against-block]
   (let [[x y] (:pos block)
         [ax ay] (:pos against-block)
         new-pos (case dir
                   :left [(inc ax) y]
                   :right [(dec ax) y]
                   :up [x (inc ay)]
                   :down [x (dec ay)])]
     (assoc block :pos new-pos)))

(defn slide-row
  "Performs the sliding algorithm on a row of blocks."
  [row dir grid]
  (let [[row combined counter _]
        (reduce
         (fn [[row combined counter allow-merge] block]
           (cond
             (empty? row)
             (let [new-block (slide-block-to-edge block dir grid)]
               [[new-block] combined counter true])

             (and allow-merge (= (:value (peek row)) (:value block)))
             (let [new-block (-> (peek row)
                                 (assoc :id counter)
                                 (update :value * 2))]
               [(conj (pop row) new-block)
                (concat combined
                        [(assoc block :merged-into counter)
                         (assoc (peek row) :merged-into counter)])
                (inc counter)
                false])

             :else
             (let [new-block (slide-block-against block dir (last row))]
               [(conj row new-block) combined counter true])))
         [[] [] (:counter grid) true]
         row)]
    [row combined counter]))

(defn slide-grid
  "Slides the blocks in the grid in the direction dir, which must be one of
  :left :right :down :up. Colliding blocks containing the same number will merge
  to form a single block containing the sum. Returns the new grid and a list of
  blocks that were merged into a new block."
  [grid dir]
  (let [rows (arrange-blocks grid (opposite-direction dir))
        initial-accumulator [(assoc grid :blocks []) []]]
    (reduce
     (fn [[grid combined] row]
       (let [[new-row more-combined new-counter] (slide-row row dir grid)
             points-scored (reduce + (map :value more-combined))
             new-grid (-> grid
                          (assoc :counter new-counter)
                          (update :blocks concat new-row)
                          (update :score + points-scored))
             new-combined (concat combined more-combined)]
         [new-grid new-combined]))
     initial-accumulator
     rows)))

; TODO: return allowed directions (so that no-op dir can't be played)
(defn impasse?
  "Returns true if the grid is full and no moves are possible. Also returns true
  when grid is nil because no move can be made on a nil grid."
  [grid]
  (or (not grid)
      (and (grid-full? grid)
           (every? #(= grid (move-grid grid %))
                     [:left :right :down :up]))))
