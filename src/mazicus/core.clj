(ns mazicus.core
  (:gen-class)
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:require [quil.core :as q])
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)])
)

;; We'll request to send `profile` stats to `println`:
(tufte/add-basic-println-handler! {})

(use 'clojure.pprint)
(use 'common)
(use 'binmaze)
(use 'sidewinder)
(use 'graph)
(use 'aldousbroder)
(use 'wilson)
(use 'huntnkill)
(use 'recursivebacktracker)
(use '[clojure.string :only (join)])

(declare draw_maze)

(def algorithms #{:binary :sidewinder :aldousbroder :wilson :huntnkill :backtrack})

(defn draw [maze]
  (q/background 255)
  (draw_maze maze)
  (comment
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
   (doseq [t (range 0 100 0.01)]
     (q/point (* t (q/sin t))
              (* t (q/cos t))))))
  )

(defn setup [maze]
  (q/frame-rate 1)
  (q/background 200)
)

(def cli-options
  [["-a" "--algorithm NAME" "Algorithm name"
      :default :binary
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
    :aldousbroder (carve_aldbro_maze size)
    :wilson (carve_wilson_maze size)
    :huntnkill (carve_huntnkill_maze size)
    :backtrack (carve_recursive_backtrack_maze size)
    :default (carve_bin_alg_maze size))
)

(defn neighbour [cell, direction, size]
  (case direction
    :left [(dec (:x cell)) (:y cell)]
    :right [(inc (:x cell)) (:y cell)]
    :top [(:x cell) (inc (:y cell))]
    :bottom [(:x cell) (dec (:y cell))]
  )
)

(defn draw_maze_cell [cell, x, y, cell_size, size, distances]
  (let [left (neighbour cell :left size)
        right (neighbour cell :right size)
        top (neighbour cell :top size)
        bottom (neighbour cell :bottom size)
        max_val (apply max (vals distances))
        current_val (get distances cell 0)]
    (if (> current_val 0)
      (let [div_val (/ current_val max_val)]
        (q/fill 255 0 0 (* div_val 255))
        (q/no-stroke)
        (q/rect x y (:x cell_size) (:y cell_size))
      )
    )
    (q/stroke-weight 2)
    (q/stroke 0 0 0)
    (if (not (is_valid_cell (get left 0) (get left 1) size))
      (q/line x y x (+ y (:y cell_size)))
    )
    (if (not (is_valid_cell (get bottom 0) (get bottom 1) size))
      (q/line x (+ y  (:y cell_size)) (+ x (:x cell_size)) (+ y (:y cell_size)))
    )
    (if (not (some #(= % top) (:neighbours cell)))
      (q/line x y (+ x (:x cell_size)) y)
    )
    (if (not (some #(= % right) (:neighbours cell)))
      (q/line (+ x (:x cell_size)) y (+ x (:x cell_size)) (+ y (:y cell_size)))
    )
  )
)

(defn draw_maze_row [row, row_y, cell_size, size, distance]
  (doall (map #(draw_maze_cell % (* (:x %) (:x cell_size)) row_y cell_size size distance) (map #(get row %) (sort (keys row)))))
)

(defn draw_maze [data]
  (let [[maze, dkstr] data
        row_keys (reverse (sort (keys (:cells maze))))
        size (count (keys (:cells maze)))
        cell_size {:x (/ 550 size) :y (/ 550 size)}
        initial_row_y 0]
    (loop [current_key (first row_keys)
           row_keys (into [] (rest row_keys))
           row_y initial_row_y]
      (draw_maze_row (get (:cells maze) current_key) row_y cell_size (count (keys (:cells maze))) dkstr)
      (if (not (empty? row_keys))
        (recur (first row_keys) (rest row_keys) (+ row_y (:y cell_size)))
        nil
      )
    )
  )
)

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
        :draw (partial draw [maze, dkstr])
        :size [800 600]
      )
    )
  )
)
