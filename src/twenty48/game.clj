;;; Copyright 2019 Mitchell Kember. Subject to the MIT License.

(ns twenty48.game
  "Defines the game screen, where all the fun happens."
  (:require [clojure.string :refer [upper-case]]
            [clojure.math.numeric-tower :as math]
            [seesaw.action :as a]
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

;;;;; Game state

(def animation-delay 20)
(def animation-increment 0.2)

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
  [state sys dir]
  (let [options (:options @sys)
        old-grid (:grid state)
        [post-slide-grid combined] (r/slide-grid old-grid dir)
        grid (insert-blocks post-slide-grid options)
        dirs (set (r/available-slide-directions grid))]
    (if (empty? dirs)
      (do (ui/show-view! sys (game-over-view sys (:score grid)))
          (initial-game-state options))
      {:grid grid
       :dirs dirs
       :old-grid old-grid
       :combined combined
       :animation {:phase :slide, :t 0.0}})))

(defn step-animation
  "Advances the animation state. Returns nil when the animation is finished."
  [animation]
  (when-let [{phase :phase t :t} animation]
    (if (< t 1)
      {:phase phase :t (c/clamp (+ t animation-increment) 0 1)}
      (case phase
        :slide {:phase :appear, :t 0.0}
        nil))))

;;;;; 2048 canvas

(def cell-gap 10)
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

(defn block-font-size
  "Returns the font size for the given block value."
  [grid-side value]
  (* (condp >= value
       64 55
       512 45
       2048 35
       30)
     (/ 4.0 grid-side)))

(defn draw-centred-text
  "Draws text centred at the given coordinates."
  [^java.awt.Graphics2D g text x y]
  (let [metrics (.getFontMetrics g)
        x' (c/round (- x (/ (.stringWidth metrics text) 2)))
        y' (c/round (+ (- y (/ (.getHeight metrics) 2))
                               (.getAscent metrics)))]
    (.drawString g text x' y')))

(defn draw-round-rect-centred
  "Draws a rounded rectangle centred at the given coordinates."
  [^java.awt.Graphics2D g x y w h]
  (.fillRoundRect g (- x (/ w 2)) (- y (/ h 2)) w h
                  cell-corner-radius cell-corner-radius))

(defn interpolate-blocks
  "Linearly interpolate block positions between two grids."
  [t old-blocks new-blocks combined]
  (let [all-blocks (concat new-blocks combined)
        id-map (zipmap (map :id all-blocks) all-blocks)]
    (map
     (fn [block]
       (let [[x1 y1] (:pos block)
             [x2 y2] (:pos (get id-map (:id block)))]
         (assoc block :pos [(c/interpolate t x1 x2)
                            (c/interpolate t y1 y2)])))
     old-blocks)))

(defn paint-canvas
  "Paints the grid in the canvas c with graphics context g."
  [state c ^java.awt.Graphics2D g]
  (let [grid (:grid state)
        animation (:animation state)
        side (:side grid)
        cell-w (cell-dimension (s/width c) side)
        cell-h (cell-dimension (s/height c) side)
        mult-x (+ cell-w cell-gap)
        mult-y (+ cell-h cell-gap)
        x-pos (fn [x] (+ cell-gap (/ cell-w 2) (* x mult-x)))
        y-pos (fn [y] (+ cell-gap (/ cell-h 2) (* y mult-y)))
        draw-block (fn [g x y m]
                     (draw-round-rect-centred
                      g (x-pos x) (y-pos y) (* m cell-w) (* m cell-h)))
        block-set (if (= (:phase animation) :slide)
                    (interpolate-blocks (:t animation)
                                        (:blocks (:old-grid state))
                                        (:blocks grid)
                                        (:combined state))
                    (:blocks grid))
        block-size-mult (if-not (= (:phase animation) :appear)
                          (constantly 1)
                          (let [appear-ids
                                (set (map :merged-into (:combined state)))]
                            (fn [block]
                              (if-not (appear-ids (:id block))
                                1
                                (- 1.25 (math/expt (- (:t animation) 0.5) 2))))))]
    (.setFont g (f/font :name :sans-serif :style :bold))
    (.setColor g (block-color-bg 0))
    (doseq [x (range side)
            y (range side)]
      (draw-block g x y 1))
    (doseq [block block-set]
      (let [value (:value block)
            [x y] (:pos block)]
        (doto g
          (.setColor (block-color-bg value))
          (draw-block x y (block-size-mult block))
          (.setColor (block-color-fg value))
          (.setFont
           (.. g getFont (deriveFont (float (block-font-size side value)))))
          (draw-centred-text (str value) (x-pos x) (y-pos y)))))))

(defn make-canvas
  "Creates the main canvas given the state atom."
  [state]
  (s/canvas :background "#bbada0"
            :paint #(paint-canvas @state %1 %2)))

;;;;; Binding and mapping

(defn bind-val-to-property
  "Binds the value associated with the key k in the source map atom to the
  property p of the target widget."
  [source k target p]
  (b/bind source
          (b/transform k)
          (b/property target p)))

(defn perform-slide
  "Action for when the user presses an arrow key to make the next move. Updates
  the game state and starts an animation timer."
  [sys state canvas dir]
  (when ((:dirs @state) dir)
    (let [handler (fn [event]
                    (swap! state update :animation step-animation)
                    (s/repaint! canvas)
                    (when-not (:animation @state)
                      (.. event getSource stop)))
          timer (javax.swing.Timer. 0 (a/action :handler handler))]
      (swap! state (fn [s]
                     (when-let [t (:timer s)] (.stop t))
                     (-> s (update-state sys dir) (assoc :timer timer))))
      (doto timer (.setDelay animation-delay) .start))))

(defn map-arrow-keys
  "Maps the arrow keys in the panel so that they update the grid atom and
  repaint the canvas."
  [sys state panel canvas]
  (doseq [dir [:left :right :up :down]]
    (k/map-key
     panel
     (upper-case (name dir))
     (fn [_] (perform-slide sys state canvas dir))
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
