(ns mazicus.core
  (:gen-class)
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:require [quil.core :as q])
)

(use 'clojure.pprint)
(use 'common)
(use 'simple)
(use 'binmaze)
(use 'sidewinder)
(use '[clojure.string :only (join)])

(def algorithms #{:binary :sidewinder})

(defn draw []
  (q/background 255)
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
   (doseq [t (range 0 100 0.01)]
     (q/point (* t (q/sin t))
              (* t (q/cos t))))))

(defn setup []
  (q/frame-rate 1)
  (q/background 200)
)

(def cli-options
  [["-a" "--algorithm NAME" "Algorithm name"
      :default "binary"
      :parse-fn #(keyword %)
      :validate [#(contains? algorithms %) (str "Must be one of given names: " (join ", " (map name algorithms)))]]
   ["-s" "--size SIZE" "Maze size"
      :default 10
      :parse-fn #(Integer/parseInt %)
      :validate [#(< % 1000) "Must be a number between 0 and 100"]]
   ["-h" "--help"]])


(defn generate_maze [algorithm, size]
  (case algorithm
    :binary (carve_bin_alg_maze size)
    :sidewinder (generate_sidewinder_path size)
    :default (carve_bin_alg_maze size)
  )
)

(defn -main [& args] 
  (let [opts (parse-opts args cli-options)
        algorithm (get-in opts [:options :algorithm])
        size (get-in opts [:options :size])
        maze (generate_maze algorithm size)]
    (q/sketch :title "Mazicus!" :setup setup :draw draw)
  )
)
