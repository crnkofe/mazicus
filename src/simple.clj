(ns simple)

(use 'common)
(use 'clojure.pprint)

(defn generate_ne[x, y, size]
  (filter 
   #(is_valid_cell (get % 0) (get % 1) size) 
   [
    [(inc x) y] [x (inc y)]
   ])
)

(defn carve_binary_cell [idx cell, size]
  {
    idx 
    (let [neighbours (generate_ne (:x cell) (:y cell) size)]
      (if (empty? neighbours)
        cell
        (let [ne_neighbour (rand-nth neighbours)]
          (assoc-in cell [:neighbours] [ne_neighbour])
        )
      )
    )
  }
)

(defn carve_binary_row [idx row, size]
  [idx (reduce merge (map #(carve_binary_cell % (get row %) size) (keys row)))]
)

(defn carve_binary_wall_optimized [graph, size]
  (into {} (map #(carve_binary_row % (get graph %) size) (keys graph)))
)

(comment "this code doesn't scale well and produces a stack overflow")
(defn carve_binary_wall [from_list, visited, graph, size]
  (if (empty? from_list)
    (let [shuffle_from (shuffle from_list)
          rnd_from (first shuffle_from)
          neighbours (generate_ne (:x rnd_from) (:y rnd_from) size)
          ]
      (if (not (empty? neighbours))
        (let [ne_neighbour (rand-nth neighbours)
              filtered_from (filter #((and (= (:x rnd_from) (:x %)) (= (:y rnd_from) (:y %)))) from_list)
              existing_neighbours (get-in graph [(:y rnd_from) (:x rnd_from) :neighbours])
              updated_graph (assoc-in graph [(:y rnd_from) (:x rnd_from) :neighbours] (into [] (conj existing_neighbours ne_neighbour)))]
          (carve_binary_wall (rest shuffle_from) (conj visited rnd_from) updated_graph size)
        )
        (carve_binary_wall (rest shuffle_from) (conj visited rnd_from) graph size)
      )
    )
    graph
  )
)

(defn generate_subindices [idx, size] 
  (into [] (reduce concat (map #(vector [idx %]) (range size))))
)

(defn generate_indices [size]
  (into [] (reduce concat (map #(generate_subindices % size) (range size))))
)

(defn carve_binary_maze [size]
  (let [maze (grid size)
        from (generate_indices size)
        ]
    (carve_binary_wall (map #(point (get % 0) (get % 1) maze) from) #{} maze size)
  )
)

(defn carve_binary_maze_optimized [size]
  (let [maze (grid size)]
    (carve_binary_wall_optimized maze size)
  )
)

(defn str_binary_maze [size]
  (print_grid (carve_binary_maze size))
)

(defn str_binary_maze_optimized [size]
  (print_grid (carve_binary_maze_optimized size))
)
