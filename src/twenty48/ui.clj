;;; Copyright 2019 Mitchell Kember. Subject to the MIT License.

(ns twenty48.ui
  (:require [clojure.java.io :as io]
            [seesaw.core :as s]
            [seesaw.icon :as i]
            [twenty48.common :as c]))

;;;;; Frame and views

(defn frame
  "Creates the application's one and only frame."
  [env]
  (s/frame :title "Twenty48"
           :size [600 :by 700]
           :on-close ({:dev :hide, :prod :exit} env)))

(defn show-view!
  "Shows a new view."
  [sys view]
  (s/config! (:frame @sys) :content view)
  (s/request-focus! view))

(defn show-view-by-id!
  "Gets the view with the given identifier from the system and shows it."
  [sys id]
  (show-view! sys (force (get-in @sys [:views id]))))

;;;;; HiDPI display support

(defn display-scale-factor
  "Returns the display scale factor (1 for non-HiDPI displays)."
  []
  (let [device (.. java.awt.GraphicsEnvironment
                   getLocalGraphicsEnvironment
                   getDefaultScreenDevice)]
    (try
      (.getScaleFactor device)
      (catch IllegalArgumentException e 1))))

(defn hidpi-icon
  "Creates an ImageIcon with HiDPI support using the @2x image file."
  [filename extension]
  (let [scale-factor (display-scale-factor)]
    (if (<= scale-factor 1)
      (-> (str filename "." extension) io/resource io/file i/icon)
      (let [image-2x (-> (str filename "@2x." extension)
                         io/resource
                         javax.imageio.ImageIO/read)]
        (proxy [javax.swing.ImageIcon] [image-2x]
          (getIconWidth []
            (c/round (/ (proxy-super getIconWidth) scale-factor)))
          (getIconHeight []
            (c/round (/ (proxy-super getIconHeight) scale-factor)))
          (paintIcon [c ^java.awt.Graphics2D g x y]
            (let [^java.awt.Graphics2D g2 (.create g)
                  recip (/ 1.0 scale-factor)]
              (.scale g2 recip recip)
              (proxy-super
               paintIcon c g2 (* scale-factor x) (* scale-factor y))
              (.dispose g2))))))))

;;;;; User interface elements

(defn label
  "Creates a label with the given text and font size. Extra arguments are passed
  on to seesaw.core/label (they must be key/value pairs)."
  [text size & more]
  (apply s/label
         :text text
         :font {:name "Lucida Grande" :size size}
         more))

(defn slider
  "Creates a slider with the most important options set. Extra arguments are
  passed on to seesaw.core/slider (they must be key/value pairs)."
  [from to major minor & more]
  (apply s/slider
         :min from
         :max to
         :major-tick-spacing major
         :minor-tick-spacing minor
         :snap-to-ticks? true
         :paint-labels? true
         :paint-ticks? true
         more))

(defn image
  "Loads an image located at the given path relative to the resouces folder.
  Returns a component containing the image. Extra arugments are passed on to
  seesaw.core/label (they must be key/value pairs)."
  [filename extension & more]
  (apply s/label
         :icon (hidpi-icon filename extension)
         more))

(defn button
  "Creates a button with an action handler. Extra arugments are passed on to
  seesaw.core/button (they must be key/value pairs)."
  [text action & more]
  (apply s/button
         :text text
         :size [80 :by 40]
         :listen [:action action]
         more))

(defn nav-button
  "Creates a button for navigating to the screen with the given identifer."
  [sys id text]
  (letfn [(action [_] (show-view-by-id! sys id))]
    (s/button :text text
              :size [80 :by 40]
              :listen [:action action])))
