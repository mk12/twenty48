;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.

(ns twenty48.core
  (:gen-class))

;;; TODO
;;; deploy => :on-close :exit
;;; app icon

(defn -main [& args]
  (s/native!)
  (s/invoke-later (s/show! (make-frame))))
