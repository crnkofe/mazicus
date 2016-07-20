(ns binmaze)

(use 'common)
(use 'clojure.pprint)
(use '[clojure.set :only (difference)])


(defn generate_bin_ne[x, y, size]
  (filter 
   #(is_valid_cell % size)
   [
    [(inc x) y] [x (inc y)]
   ])
)

(defn next_unused [graph, visited]
  (if (>= (count (distinct visited)) (* (:size graph) (:size graph)))
    nil 
    (let [all_indices (map coords (all_nodes graph))
          differ (into [] (difference (into #{} all_indices) (into #{} visited)))]
      (if (empty? differ)
        nil
        (rand-nth differ)
      )
    )
  )
)

(defn next_step [cell, graph, visited]
  (let [neighbours (generate_bin_ne (:x cell) (:y cell) (:size graph))]
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
      (let [ne_neighbour (rand-nth neighbours)]
        [(point ne_neighbour graph)
         (assoc-in graph [:cells (:y cell) (:x cell) :neighbours] [ne_neighbour])
         (conj visited ne_neighbour)]
      )
    )
  )
)

(defn carve_bin_alg_maze [size]
  (let [initial_maze (grid size)
        initial_cell (point [0 0] initial_maze)]
    (loop [current_cell initial_cell
           updated_maze initial_maze
           current_visited [(coords initial_cell)]]
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
