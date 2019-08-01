;;; Copyright 2019 Mitchell Kember. Subject to the MIT License.

(ns twenty48.common
  "Some common functions used by other modules."
  (:require [clojure.math.numeric-tower :as math]))

(defn round
  "Rounds a number to the nearest integer."
  [x]
  (int (math/round x)))
