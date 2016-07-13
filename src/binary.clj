(ns binary)

(use 'common)
(use 'clojure.pprint)

(defn generate_bin_ne[x, y, size]
  (filter 
   #(is_valid_cell (get % 0) (get % 1) size) 
   [
    [(inc x) y] [x (inc y)]
   ])
)

(defn carve_bin_alg_cell [idx, cell, size]
  {
    idx 
    (let [neighbours (generate_bin_ne (:x cell) (:y cell) size)]
      (if (empty? neighbours)
        cell
        (let [ne_neighbour (rand-nth neighbours)]
          (assoc-in cell [:neighbours] [ne_neighbour])
        )
      )
    )
  }
)

(defn next_cell [cell, graph, size]
  (let [neighbours (generate_bin_ne (:x cell) (:y cell) size)]
    (if (empty? neighbours)
      [nil, graph]
      (let [ne_neighbour (rand-nth neighbours)]
        [(get-in graph [(get ne_neighbour 1) (get ne_neighbour 0)]) 
         (assoc-in graph [(:y cell) (:x cell) :neighbours] [ne_neighbour])]
      )
    )
  )
)

(defn carve_bin_alg_maze [size]
  (let [initial_maze (grid size)
        initial_cell (get-in initial_maze [0 0])]
    (loop [current_cell initial_cell
           updated_maze initial_maze]
      (if-not (= current_cell nil)
        (let [[cell, maze] (next_cell current_cell updated_maze size)]
          (recur cell maze)
        )
        updated_maze
      )
    )
  )
)

(defn str_bin_algo_maze [size]
  (print_grid (carve_bin_alg_maze size))
)
