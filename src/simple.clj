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

(defn carve_binary_cell [idx, cell, size]
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

(defn carve_binary_row [idx, row, size]
  [idx (reduce merge (map #(carve_binary_cell % (get row %) size) (sort (keys row))))]
)

(defn carve_binary_wall [graph, size]
  (into {} (map #(carve_binary_row % (get graph %) size) (sort (keys graph))))
)

(defn carve_binary_maze [size]
  (let [maze (grid size)]
    (carve_binary_wall maze size)
  )
)

(defn str_binary_maze [size]
  (print_grid (carve_binary_maze size))
)
