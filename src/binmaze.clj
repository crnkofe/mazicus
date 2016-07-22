(ns binmaze)

(use 'common)
(use 'clojure.pprint)
(use '[clojure.set :only (difference)])

(defn generate_bin_ne[cell, maze]
  (let [coords (into [] (concat (neighbours_direction cell :north) (neighbours_direction cell :east)))]
    (filter #(within_bounds maze %) coords)
  )
)

(defn next_unused [graph, visited]
  (if (>= (count visited) (* (:size graph) (:size graph)))
    nil 
    (let [differ (into [] (difference (:all_indices graph) visited))]
      (if (empty? differ)
        nil
        (rand-nth differ)
      )
    )
  )
)

(defn next_step [cell, graph, visited]
  (let [neighbours (generate_bin_ne cell graph)]
    (if (empty? neighbours)
      (let [unvisited_cell (next_unused graph visited)]
        (if (not (= unvisited_cell nil))
          (next_step (point unvisited_cell graph) graph (conj visited unvisited_cell))
          [nil graph visited]
        )
        [(point unvisited_cell graph)
         graph
         (conj visited unvisited_cell)]
      )
      (let [ne_neighbour (rand-nth neighbours)
            unvisited_cell (next_unused graph visited)]
        (if (not (= unvisited_cell nil))
          [(point unvisited_cell graph)
           (connect_nodes cell (point ne_neighbour graph) graph )
           (conj visited unvisited_cell)]
          [nil (connect_nodes cell (point ne_neighbour graph) graph) visited]
        )
      )
    )
  )
)

(defn carve_bin_alg_maze [size, grid_type]
  (let [initial_maze (generate_grid size grid_type)
        initial_cell (random_point initial_maze)]
    (loop [current_cell initial_cell
           updated_maze initial_maze
           current_visited #{(coords initial_cell)}]
      (if-not (= current_cell nil)
        (let [[cell maze visited] (next_step current_cell updated_maze current_visited)]
          (recur cell maze visited)
        )
        updated_maze
      )
    )
  )
)

(defn str_bin_algo_maze [size]
  (print_grid (carve_bin_alg_maze size))
)
