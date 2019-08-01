;;; Copyright 2019 Mitchell Kember. Subject to the MIT License.

(ns twenty48.core
  (:require [twenty48.system :as s])
  (:gen-class))

(defn -main [& args]
  (let [env (if (= (args 0) "dev") :dev :prod)]
    (s/start (s/system env))))
