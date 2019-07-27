;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.

(ns twenty48.core
  (:require [seesaw.core :as s]
    [twenty48.ui :as ui])
  (:gen-class))

;;; TODO
;;; deploy => :on-close :exit
;;; app icon

(defn -main [& args]
  (s/native!)
  (s/invoke-later (s/show! (ui/frame))))
