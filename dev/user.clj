;;; Copyright 2019 Mitchell Kember. Subject to the MIT License.

;;; This code is taken from Stuart Sierra's blog post:
;;; http://thinkrelevance.com/blog/2013/06/04/clojure-workflow-reloaded

(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [twenty48.system :as s]))

(def system nil)

(defn init []
  (alter-var-root
    #'system
    (constantly (s/system :dev))))

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
