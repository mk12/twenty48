;;; Copyright 2019 Mitchell Kember. Subject to the MIT License.

(ns twenty48.grid
  "Implements the 2048 grid model.")

(defn empty-grid
  "Returns an empty grid with side length n."
  [n]
  {:side n      ; side length
   :score 0     ; sum of merged block values
   :counter 0   ; counter for block IDs
   :blocks []}) ; active blocks

(defn matrix->grid
  "Creates a grid from a 2D vector of numbers (0 means empty)."
  [matrix]
  (let [[counter blocks]
        (->> matrix
             (map-indexed
              (fn [y row] (map-indexed (fn [x value] [[x y] value]) row)))
             (apply concat)
             (reduce
              (fn [[counter blocks] [pos value]]
                (if (zero? value)
                  [counter blocks]
                  [(inc counter)
                   (conj blocks {:id counter :value value :pos pos})]))
              [0 []]))]
    {:side (count matrix)
     :score 0
     :counter counter
     :blocks blocks}))

(defn grid->matrix
  "Converts a grid to a 2D vector of numbers (0 means empty)."
  [grid]
  (let [indices (range (:side grid))
        blocks (:blocks grid)
        pos-map (zipmap (map :pos blocks) blocks)]
    (mapv
     (fn [y]
       (mapv (fn [x]
               (or (:value (pos-map [x y])) 0))
             indices))
     indices)))

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
  calling insert-one-block. Does not insert all of them if there are not enough
  empty spaces in the grid."
  [grid numbers]
  (reduce
   (fn [g n] (or (insert-one-block g n) (reduced g)))
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
    (map #(keep pos-map %) coords)))

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

(defn available-slide-directions
  "Returns directions where sliding is no a no-op."
  [grid]
  (remove
   #(= (-> grid :blocks set)
       (-> grid (slide-grid %) (get 0) :blocks set))
   [:left :right :up :down]))
