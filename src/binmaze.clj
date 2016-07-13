(ns binmaze)

(use 'common)
(use 'clojure.pprint)
(use '[clojure.set :only (difference)])


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

(defn next_unused [graph, size, visited]
  (if (>= (count (distinct visited)) (* size size))
    nil 
    (let [all_indices (map #(vector (:x %) (:y %)) (reduce concat (map vals (vals graph))))
          differ (into [] (difference (into #{} all_indices) (into #{} visited)))]
      (if (empty? differ)
        nil
        (rand-nth differ)
      )
    )
  )
)

(defn next_step [cell, graph, visited, size]
  (let [neighbours (generate_bin_ne (:x cell) (:y cell) size)]
    (if (empty? neighbours)
      (let [unvisited_cell (next_unused graph size visited)]
        (if (not (= unvisited_cell nil))
          (next_step (get-in graph [(get unvisited_cell 1) (get unvisited_cell 0)]) graph (conj visited unvisited_cell) size)
          [nil graph visited]
        )
        [(get-in graph [(get unvisited_cell 1) (get unvisited_cell 0)])
         graph
         (conj visited unvisited_cell)]
      )
      (let [ne_neighbour (rand-nth neighbours)]
        [(get-in graph [(get ne_neighbour 1) (get ne_neighbour 0)])
         (assoc-in graph [(:y cell) (:x cell) :neighbours] [ne_neighbour])
         (conj visited ne_neighbour)]
      )
    )
  )
)

(defn carve_bin_alg_maze [size]
  (let [initial_maze (grid size)
        initial_cell (get-in initial_maze [0 0])]
    (loop [current_cell initial_cell
           updated_maze initial_maze
           current_visited [[(:x initial_cell) (:y initial_cell)]]]
      (if-not (= current_cell nil)
        (let [[cell maze visited] (next_step current_cell updated_maze current_visited size)]
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
