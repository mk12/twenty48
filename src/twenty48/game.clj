;;; Copyright 2019 Mitchell Kember. Subject to the MIT License.

(ns twenty48.game
  "Defines the game screen, where all the fun happens."
  (:require [clojure.string :refer [upper-case]]
            [clojure.math.numeric-tower :as math]
            [seesaw.bind :as b]
            [seesaw.color :as color]
            [seesaw.core :as s]
            [seesaw.font :as f]
            [seesaw.keymap :as k]
            [seesaw.mig :as m]
            [twenty48.common :as c]
            [twenty48.grid :as r]
            [twenty48.ui :as ui]))

;;;;; Forward declarations

(declare game-over-view)

;;;;; Grid

(defn make-grid
  "Creates a new empty grid according to the option values."
  [options]
  (r/empty-grid (:grid-size options)))

(defn new-block-number
  "Returns a random new number to be added to the grid."
  [options]
  (let [p (/ (:probability options) 100.0)
        max-exponent (:largest-new options)]
    (loop [exponent 1]
      (if (or (= exponent max-exponent) (< (rand) p))
        (math/expt 2 exponent)
        (recur (inc exponent))))))

(defn insert-blocks
  "Inserts new blocks into the grid according to the options."
  [grid options]
  (r/insert-blocks
   grid
   (take (:at-a-time options)
         (repeatedly #(new-block-number options)))))

(defn move-and-insert
  "Moves the grid in the direction dir and inserts numbers in empty cells of the
  grid. The number of insertions is controlled by the option :at-a-time."
  [options grid dir]
  (-> grid
      (r/slide-grid dir)
      (get 0)
      (insert-blocks options)))

;;;;; Game state

(defn initial-game-state
  "Returns the initial game state."
  [options]
  {:grid (insert-blocks (make-grid options) options)
   :dirs #{:left :right :up :down}})

(defn start-game
  "Starts a new game."
  [sys state canvas]
  (reset! state (initial-game-state (:options @sys)))
  (s/repaint! canvas))

(defn update-state
  "Updates the given state by moving the grid in the direction dir and adding a
  new block. Returns the new state Switches to the game-over screen when the
  grid is full and no more moves are possible."
  [sys state dir]
  (let [options (:options @sys)
        new-grid (move-and-insert options (:grid state) dir)
        new-dirs (set (r/available-slide-directions new-grid))]
    (if (empty? new-dirs)
      (do (ui/show-view! sys (game-over-view sys (:score new-grid)))
          (initial-game-state options))
      {:grid new-grid :dirs new-dirs})))

;;;;; 2048 canvas

(def cell-gap 10)
(def cell-font-size 50)
(def cell-corner-radius 15)

(defn cell-dimension
  "Calculates the size of a cell along one dimension when there are to be n
  cells evenly spaced by cell-gap in a canvas with size canvas-dim, with gaps
  between the border and edge cells as well."
  [canvas-dim n]
  (/ (- canvas-dim
        (* (inc n) cell-gap))
     n))

(defn block-color-bg
  "Returns the background color to use for the given block value."
  [value]
  (-> {0 "#cdc1b5"
       2 "#eee4da"
       4 "#ede0c8"
       8 "#f2b179"
       16 "#f59563"
       32 "#f67c5f"
       64 "#f65e3b"
       128 "#edcf72"
       256 "#edcc61"
       512 "#edc850"
       1024 "#edc53f"
       2048 "#edc22e"}
      (get value "#3c3a32")
      color/color))

(defn block-color-fg
  "Returns the foreground color to use for the given block value."
  [value]
  (color/color (if (<= value 4) "#776e65" "#f9f6f2")))

(defn draw-centred-text
  "Draws text centred at the given coordinates."
  [^java.awt.Graphics2D g text x y]
  (let [metrics (.getFontMetrics g)
        x' (c/round (- x (/ (.stringWidth metrics text) 2)))
        y' (c/round (+ (- y (/ (.getHeight metrics) 2))
                               (.getAscent metrics)))]
    (.drawString g text x' y')))

(defn paint-canvas
  "Paints the grid in the canvas c with graphics context g."
  [grid c ^java.awt.Graphics2D g]
  (let [side (:side grid)
        cell-w (cell-dimension (s/width c) side)
        cell-h (cell-dimension (s/height c) side)
        mult-x (+ cell-w cell-gap)
        mult-y (+ cell-h cell-gap)
        x-pos (fn [x] (+ cell-gap (* x mult-x)))
        y-pos (fn [y] (+ cell-gap (* y mult-y)))
        draw-block (fn [g x y]
                     (.fillRoundRect g (x-pos x) (y-pos y)
                                     cell-w cell-h
                                     cell-corner-radius cell-corner-radius))]
    (.setFont g (f/font :name :sans-serif
                        :style :bold
                        :size cell-font-size))
    (.setColor g (block-color-bg 0))
    (doseq [x (range side)
            y (range side)]
      (draw-block g x y))
    (doseq [block (:blocks grid)]
      (let [[x y] (:pos block)]
        (doto g
          (.setColor (block-color-bg (:value block)))
          (draw-block x y)
          (.setColor (block-color-fg (:value block)))
          (draw-centred-text
           (str (:value block))
           (+ (x-pos x) (/ cell-w 2))
           (+ (y-pos y) (/ cell-h 2))))))))

(defn make-canvas
  "Creates the main canvas given the state atom."
  [state]
  (s/canvas :background "#bbada0"
            :paint #(paint-canvas (:grid @state) %1 %2)))

;;;;; Binding and mapping

(defn bind-val-to-property
  "Binds the value associated with the key k in the source map atom to the
  property p of the target widget."
  [source k target p]
  (b/bind source
          (b/transform k)
          (b/property target p)))

(defn map-arrow-keys
  "Maps the arrow keys in the panel so that they update the grid atom and
  repaint the canvas."
  [sys state panel canvas]
  (doseq [k [:left :right :up :down]]
    (k/map-key
     panel
     (upper-case (name k))
     (fn [_]
       (when ((:dirs @state) k)
         (swap! state #(update-state sys % k))
         (s/repaint! canvas)))
     :scope :descendants)))

;;;;; Game view

(defn game-panel
  "Helper function for making the game panel."
  [sys state score-label canvas]
  (m/mig-panel
    :id :game
    :constraints ["fill"
                  "[fill,grow]"
                  "[c,nogrid]10[fill,grow]"]
    :items [[(ui/image "logo" "png") "ax l"]
            [:fill-h "w 3"]
            [:fill-h "growx"]
            [score-label]
            [:fill-h "growx"]
            [(ui/button "New Game" (fn [_] (start-game sys state canvas))) "ax r"]
            [(ui/nav-button sys :options "Options") "ax r, wrap"]
            [canvas "hmin 100"]]
    :focusable? true
    :listen [:component-resized
             (fn [_] (s/repaint! canvas))]))

(defn game-view
  "Creates the game view, which contains a few UI elements and the 2048 grid."
  [sys]
  (let [state (atom (initial-game-state (:options @sys)))
        score-label (ui/label 0 40)
        canvas (make-canvas state)
        panel (game-panel sys state score-label canvas)]
    (bind-val-to-property state (comp :score :grid) score-label :text)
    (map-arrow-keys sys state panel canvas)
    panel))

;;;;; Game over view

(defn game-over-view
  "Create the game-over view, which displays the score and allows the user to
  begin a new game."
  [sys score]
  (m/mig-panel
   :id :game-over
   :constraints ["wrap 1, fill"
                 "[c]"
                 "push[]50[]50[]push"]
   :items [[(ui/label "Game Over" 40)]
           [(ui/label score 60)]
           [(ui/nav-button sys :game "Try Again")]]))
