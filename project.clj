(defproject mazicus "0.1.0-SNAPSHOT"
  :description "This project features a number of algorithms to generate mazes (according to Mazes for Programmers book)"
  :url "https://github.com/crnkofe/mazicus"
  :license {}
  :dependencies [[org.clojure/clojure "1.8.0"], [org.clojure/tools.namespace "0.2.11"] ]
  :main ^:skip-aot mazicus.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
