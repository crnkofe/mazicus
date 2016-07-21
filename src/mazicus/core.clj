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

(def cli-options
  [["-a" "--algorithm NAME" "Algorithm name"
      :default :binary
      :parse-fn #(keyword %)
      :validate [#(contains? algorithms %) (str "Must be one of given names: " (join ", " (map name algorithms)))]]
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

(defn generate_maze [algorithm, size]
  (case algorithm
    :binary (carve_bin_alg_maze size)
    :sidewinder (generate_sidewinder_path size)
    :aldousbroder (carve_aldbro_maze size)
    :wilson (carve_wilson_maze size)
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
          maze (p ::algoritem (polar_grid size))]
      (q/sketch 
        :title "Mazicus!" 
        :setup (partial setup maze)
        :draw (partial draw_maze [maze, nil])
        :size [800 600]
      )
    )
  )
)

(comment
(defn -main [& args] 
  (profile
    {}
    (let [opts (parse-opts args cli-options)
          algorithm (get-in opts [:options :algorithm])
          size (get-in opts [:options :size])
          maze (p ::algoritem (generate_maze algorithm size))
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
)
