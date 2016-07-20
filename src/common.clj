(ns common)

(use 'clojure.pprint)
(use '[clojure.string :only (join split blank?)])
(use '[clojure.set :only (difference, union)])

(declare generate_neighbours)

(defrecord Cell [x, y, neighbours])

(defrecord Grid [cells, size, all_indices])

(defn point 
  ([x, y, graph] (get (get (:cells graph) y) x))
  ([point, graph] (get (get (:cells graph) (get point 1)) (get point 0)))
)

(defn generate_row [idx, size] 
  (
   into 
   (sorted-map) 
   (
    reduce conj (map #(hash-map % (->Cell % idx (generate_neighbours % idx size))) (range size))
   )
  )
)

(defn is_valid_cell 
  ([point, size] (is_valid_cell (get point 0) (get point 1) size))
  ([x, y, size] (
      cond 
        (< x 0) false
        (>= x size) false
        (< y 0) false
        (>= y size) false
        :else true
    )
  )
)

(defn is_valid_grid_cell [x, y, grid]
  (let [found (get-in grid [y x])]
    (if (= nil found)
      false
      true
    )
  )
)

(defn generate_neighbours 
  ([coords, size] 
   (generate_neighbours (get coords 0) (get coords 1) size)
  )
  ([x, y, size] 
    (filter 
      #(is_valid_cell (get % 0) (get % 1) size) 
      [
        [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]
      ]
    )
  )
)

(defn all_nodes [maze]
  (reduce concat (map vals (vals (:cells maze))))
)

(defn contains_node [node, neighbours]
  (some #(= % [(:x node) (:y node)]) neighbours)
)

(defn coords [node]
  [(:x node) (:y node)]
)

(defn all_maze_cells [maze]
  (reduce concat (map vals (vals (:cells maze))))
)

(defn connected_neighbours [node, maze]
  (let [linked_neighbours (map #(point % maze) (:neighbours node))
        neighbours (map #(point % maze) (generate_neighbours (coords node) (:size maze)))
        reverse_linked_neighbours (filter #(contains_node node (:neighbours %)) neighbours)]
    (into [] (union (into #{} linked_neighbours) (into #{} reverse_linked_neighbours)))
  )
)

(defn connect_nodes [n1, n2, maze]
  (let [idx1 [:cells (:y n1) (:x n1) :neighbours]
        idx2 [:cells (:y n2) (:x n2) :neighbours]
        maze1 (assoc-in maze idx1 (conj (get-in maze idx1) [(:x n2) (:y n2)]))]
    (assoc-in maze1 idx2 (conj (get-in maze1 idx2) [(:x n1) (:y n1)]))
  )
)

(defn connection [size]
  (into (sorted-map) (reduce conj (map #(hash-map % (generate_row % size)) (range size))))
)

(defn generate_grid_row [idx, size] 
  (
   into 
   (sorted-map) 
   (
    reduce conj (map #(hash-map % (->Cell % idx [])) (range size))
   )
  )
)

(defn filter_visited [visited, neighbours] 
  (filter #(not (contains? visited %)) neighbours)
)

(defn grid_row_rep [cell]
  (if (some #(= [(inc (:x cell)) (:y cell)] %) (:neighbours cell))
    "O-"
    "O "
  )
)

(defn grid_top_rep [cell]
  (if (some #(= [(:x cell) (inc (:y cell))] %) (:neighbours cell))
    "| "
    "  "
  )
)

(defn print_row [row]
  (let [grid_row (map #(grid_row_rep (get row %)) (sort (keys row)))
        grid_top_row (map #(grid_top_rep (get row %)) (sort (keys row)))]
     (if (not (blank? (join "" grid_top_row)))
       (join "\n" [(join "" grid_top_row) (join "" grid_row) ""])
       (str (join "" grid_row) "\n")
     )
  )
)

(defn print_grid [grid] 
  (join "" (map #(print_row (get grid %)) (reverse (sort (keys grid)))))
)

(defn grid[size]
  (let [grid (into (sorted-map) (reduce conj (map #(hash-map % (generate_grid_row % size)) (range size))))]
    (->Grid 
      grid
      size 
      (into #{} (map coords (reduce concat (map vals (vals grid)))))
    )
  )
)
