;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.

(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [twenty48.core :as c]))

(def system nil)

(defn init []
  (alter-var-root
    #'system
    (constantly (c/system))))

(defn start []
  (alter-var-root #'system c/start))

(defn stop []
  (alter-var-root
    #'system
    #(when % (c/stop %))))

(defn go [] (init) (start))

(defn reset []
  (stop)
  (refresh :after 'user/go))
