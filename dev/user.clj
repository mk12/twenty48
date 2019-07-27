;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.

(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [twenty48.system :as s]))

(def system nil)

(defn init []
  (alter-var-root
    #'system
    (constantly (s/system))))

(defn start []
  (alter-var-root #'system s/start))

(defn stop []
  (alter-var-root
    #'system
    #(when % (s/stop %))))

(defn go [] (init) (start))

(defn reset []
  (stop)
  (refresh :after 'user/go))
