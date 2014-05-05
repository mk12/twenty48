;;; Copyright 2013 Mitchell Kember. Subject to the MIT License.

(ns twenty48.game
  "Defines the game screen, where all the fun happens."
  (:require [clojure.string :refer [upper-case]]
            [seesaw.bind :as b]
            [seesaw.core :as s]
            [seesaw.graphics :as g]
            [seesaw.keymap :as k]
            [seesaw.mig :as m]
            [twenty48.common :as c]
            [twenty48.grid :as r]
            [twenty48.options :as o]))

;;;;; Grid

(def final-score (atom 0))

(defn make-grid
  "Creates a new empty grid according to the option values."
  []
  (r/make-grid (:grid-size @o/options)))

(defn new-block
  "Returns a random new number to be added to the grid."
  []
  (let [exp (-> @o/options :largest-new rand-int inc)]
    (reduce * (repeat exp 2))))

(defn move-and-insert
  "Moves the grid in the direction dir and inserts numbers in empty cells of the
  grid. The number of insertions is controlled by the option :at-a-time."
  [grid dir]
  (-> grid
      (r/move-grid dir)
      (r/insert-numbers
        (take (:at-a-time @o/options)
              (repeatedly new-block)))))

(defn update-grid
  "Updates the given grid by moving it in the direction dir and adding a new
  block. Returns the new grid. Switches to the game-over screen when the grid is
  full and no more moves are possible."
  [grid dir]
  (let [new-grid (move-and-insert grid dir)]
    (if (r/impasse? new-grid)
      (do (reset! final-score (:score grid))
          (c/show-card! :game-over)
          (make-grid))
      new-grid)))

;;;;; 2048 canvas

(def cell-gap 10)

(defn paint-canvas
  "Paints the grid in the canvas c with graphics context g."
  [grid c g]
  (g/draw g
          (g/circle (/ (s/width c) 2) (/ (s/height c) 2) 30)
          (g/style :background :red)))

(defn make-canvas
  "Creates the main canvas given a 2048 grid atom."
  [agrid]
  (s/canvas :background "#ddd"
            :paint #(paint-canvas @agrid %1 %2)))

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
  [panel canvas agrid]
  (doseq [k [:left :right :up :down]]
    (k/map-key
      panel
      (upper-case (name k))
      (fn [_]
        (swap! agrid #(update-grid % k))
        (s/repaint! canvas))
      :scope :descendants)))

;;;;; Game panel

(defn game-panel
  "Helper function for making the game panel."
  [score-label canvas]
  (m/mig-panel
    :id :game
    :constraints ["fill"
                  "[fill,grow]"
                  "[c,nogrid]10[fill,grow]"]
    :items [[(c/image "logo.png") "ax l"]
            [:fill-h "w 3"]
            [:fill-h "growx"]
            [score-label]
            [:fill-h "growx"]
            [(c/nav-button :options "Options") "ax r, wrap"]
            [canvas "hmin 100"]]
    :focusable? true
    :listen [:component-resized
             (fn [_] (s/repaint! canvas))]))

(defn make-game-panel
  "Creates the game panel, which contains a few UI elements and the 2048 grid."
  []
  (let [agrid (atom (make-grid))
        score-label (c/label 0 40)
        canvas (make-canvas agrid)
        panel (game-panel score-label canvas)]
    (bind-val-to-property agrid :score score-label :text)
    (map-arrow-keys panel canvas agrid)
    panel))

;;;;; Game over panel

(defn game-over-panel
  "Helper function for making the game-over panel."
  [score-label]
  (m/mig-panel
    :id :game-over
    :constraints ["wrap 1, fill"
                  "[c]"
                  "push[]50[]50[]push"]
    :items [[(c/label "Game Over" 40)]
            [score-label]
            [(c/nav-button :game "Try Again")]]))

(defn make-game-over-panel
  "Create the game-over panel, which displays the score and allows the user to
  begin a new game."
  []
  (let [score-label (c/label 0 60)
        panel (game-over-panel score-label)]
    (b/bind final-score (b/property score-label :text))
    panel))
