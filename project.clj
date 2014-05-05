(defproject mk12/twenty48 "42"
  :description "Just another 2048 clone"
  :url "http://github.com/mk12/twenty48"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [seesaw "1.4.4"]]
  :resource-paths ["resources"]
  :main twenty48.core
  :javac-options ["-Xdock:Twenty48"]
  :target-path "target/%s/"
  :profiles {:uberjar {:aot :all}})
