(ns mazicus.core
  (:gen-class)
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:require [quil.core :as q])
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)])
  (:use [binmaze :only (carve_bin_alg_maze)])
  (:use [sidewinder :only (generate_sidewinder_path)])
  (:use [wilson :only (carve_wilson_maze)])
  (:use [aldousbroder :only (carve_aldbro_maze)])
  (:use [huntnkill :only (carve_huntnkill_maze)])
  (:use [recursivebacktracker :only (carve_recursive_backtrack_maze)])
)

;; We'll request to send `profile` stats to `println`:
(tufte/add-basic-println-handler! {})

(use 'clojure.pprint)
(use 'drawing)
(use 'common)
(use 'graph)
(use '[clojure.string :only (join)])

(def algorithms #{:binary :sidewinder :aldousbroder :wilson :huntnkill :backtrack})
(def maze_types #{:square :polar})

(def cli-options
  [["-a" "--algorithm NAME" "Algorithm name"
      :default :binary
      :parse-fn #(keyword %)
      :validate [#(contains? algorithms %) (str "Must be one of given names: " (join ", " (map name algorithms)))]]
   ["-m" "--maze TYPE" "Maze type"
      :default :square
      :parse-fn #(keyword %)
      :validate [#(contains? maze_types %) (str "Must be one of given names: " (join ", " (map name maze_types)))]]
   ["-s" "--size SIZE" "Maze size"
      :default 10
      :parse-fn #(Integer/parseInt %)
      :validate [#(< % 1000) "Must be a number between 0 and 100"]]
   ["-h" "--help"]]
)

(defn draw_maze [data]
  (let [[maze, dkstr] data]
    (q/background 255)
    (draw maze dkstr)
    (comment
    (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
     (doseq [t (range 0 100 0.01)]
       (q/point (* t (q/sin t))
                (* t (q/cos t))))))
    )
  )

(defn setup [maze]
  (q/frame-rate 1)
  (q/background 200)
)

(defn generate_maze [algorithm, size, maze_type]
  (case algorithm
    :binary (carve_bin_alg_maze size maze_type)
    :sidewinder (generate_sidewinder_path size maze_type)
    :aldousbroder (carve_aldbro_maze size maze_type)
    :wilson (carve_wilson_maze size maze_type)
    :huntnkill (carve_huntnkill_maze size)
    :backtrack (carve_recursive_backtrack_maze size)
    :default (carve_bin_alg_maze size))
)

(defn -main [& args] 
  (profile
    {}
    (let [opts (parse-opts args cli-options)
          algorithm (get-in opts [:options :algorithm])
          size (get-in opts [:options :size])
          maze_type (get-in opts [:options :maze])
          maze (p ::algoritem (generate_maze algorithm size maze_type))
          dkstr (p ::dijsktra (dijkstra maze))
          dead_end_count (p ::dead (dead_ends maze))]
      (println "Dead ends:" dead_end_count)
      (q/sketch 
        :title "Mazicus!" 
        :setup (partial setup maze)
        :draw (partial draw_maze [maze, dkstr])
        :size [800 600]
      )
    )
  )
)
