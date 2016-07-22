(ns common)

(use 'clojure.pprint)
(use '[clojure.string :only (join split blank?)])
(use '[clojure.set :only (difference, union)])

(declare point)
(declare is_valid_cell)
(declare is_valid_polar_cell)
(declare generate_neighbours)

(defprotocol CellProtocol
  (coords [node])
  (neighbours_direction [node, direction])
)

(defprotocol GridProtocol
  (random_point [grid]) 
  (within_bounds [grid cell_coords])
  (update_neighbours [grid cell neighbours])
)

(defrecord Cell [x, y, neighbours, valid_neighbours]
  CellProtocol
  (coords [node]
    [(:x node) (:y node)]
  )
  (neighbours_direction [node, direction]
    (case direction
      :north (filter #(< (:y node) (get % 1)) (:valid_neighbours node))
      :south (filter #(> (:y node) (get % 1)) (:valid_neighbours node))
      :east (filter #(< (:x node) (get % 0)) (:valid_neighbours node))
      :west (filter #(> (:x node) (get % 0)) (:valid_neighbours node))
    )
  )
)

(defrecord Grid [cells, size, all_indices, count]
  GridProtocol
  (random_point [grid]
    (point (rand-nth (into [] (:all_indices grid))) grid)
  )
  (within_bounds [grid cell_coords]
    (is_valid_cell cell_coords (:size grid))
  )
  (update_neighbours [grid cell neighbours]
    (let [existing_neighbours (get-in grid [:cells (:y cell) (:x cell) :neighbours])]
      (assoc-in grid [:cells (:y cell) (:x cell) :neighbours] (into [] (into #{} (concat existing_neighbours neighbours))))
    )
  )
)

(defrecord PolarCell [rad, dist, neighbours, valid_neighbours]
  CellProtocol
  (coords [node]
    [(:rad node) (:dist node)]
  )
  (neighbours_direction [node, direction]
    (case direction
      :north (filter #(< (:dist node) (get % 1)) (:valid_neighbours node))
      :south (filter #(> (:dist node) (get % 1)) (:valid_neighbours node))
      :east (filter #(> (:rad node) (get % 0)) (:valid_neighbours node))
      :west (filter #(< (:rad node) (get % 0)) (:valid_neighbours node))
    )
  )
)

(defrecord PolarGrid [cells, size, all_indices, count]
  GridProtocol
  (random_point [grid]
    (point (rand-nth (into [] (:all_indices grid))) grid)
  )  
  (within_bounds [grid cell_coords]
    (is_valid_polar_cell cell_coords (:size grid))
  )
  (update_neighbours [grid cell neighbours]
    (let [existing_neighbours (get-in grid [:cells (:dist cell) (:rad cell) :neighbours])]
      (assoc-in grid [:cells (:dist cell) (:rad cell) :neighbours] (into [] (into #{} (concat existing_neighbours neighbours ))))
    )
  )
)

(defn point 
  ([x, y, graph] (get (get (:cells graph) y) x))
  ([point, graph] (get-in graph [:cells (get point 1) (get point 0)]))
)

(defn generate_neighbours [coords, size] 
  (let [x (get coords 0)
        y (get coords 1)]
    (filter 
      #(is_valid_cell (get % 0) (get % 1) size) 
      [
        [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]
      ]
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

(defn is_valid_polar_cell 
  ([coords, size] (is_valid_polar_cell (get coords 0) (get coords 1) size))
  ([angle, distance, size] (
      cond 
        (< angle 0) false
        (>= angle 360) false
        (< distance 0) false
        (>= distance size) false
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

(defn all_nodes [maze]
  (reduce concat (map vals (vals (:cells maze))))
)

(defn contains_node [node, neighbours]
  (some #(= % (coords node)) neighbours)
)

(defn all_maze_cells [maze]
  (reduce concat (map vals (vals (:cells maze))))
)

(defn connected_neighbours [node, maze]
  (let [linked_neighbours (map #(point % maze) (:neighbours node))
        neighbours (map #(point % maze) (:valid_neighbours node))
        reverse_linked_neighbours (filter #(contains_node node (:neighbours %)) neighbours)]
    (into [] (union (into #{} linked_neighbours) (into #{} reverse_linked_neighbours)))
  )
)

(defn connect_nodes [n1, n2, maze]
  (let [idx1 (concat [:cells] (coords n1) [:neighbours])
        idx2 (concat [:cells] (coords n2) [:neighbours])
        maze1 (update_neighbours maze n1 [(coords n2)])]
    (update_neighbours maze1 n2 [(coords n1)])
  )
)

(defn generate_grid_row [idx, size] 
  (
   into 
   (sorted-map) 
   (
    reduce conj (map #(hash-map % (->Cell % idx [] (generate_neighbours [% idx] size))) (range size))
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
  (let [grid (into (sorted-map) (reduce conj (map #(hash-map % (generate_grid_row % size)) (range size))))
        indices (into #{} (map coords (reduce concat (map vals (vals grid)))))]
    (->Grid 
      grid
      size 
      indices
      (count indices)
    )
  )
)

(defn generate_polar_neighbours [coords, size] 
  (let [angle (get coords 0)
        distance (get coords 1)
        angular_distance (/ 360 size)]
    (filter 
      #(is_valid_polar_cell % size) 
      [
        [(- angle angular_distance) distance] [(+ angle angular_distance) distance] [angle (dec distance)] [angle (inc distance)]
      ]
    )
  )
)

(defn generate_polar_grid_row [idx, size] 
  (
   into 
   (sorted-map) 
   (
    reduce conj (map #(hash-map % (->PolarCell % idx [] (generate_polar_neighbours [% idx] size))) (range 0 360 (/ 360 size)))
   )
  )
)

(defn polar_grid[size]
  (let [grid (into (sorted-map) (reduce conj (map #(hash-map % (generate_polar_grid_row % size)) (range size))))
        indices (into #{} (map coords (reduce concat (map vals (vals grid)))))]
    (->PolarGrid 
      grid
      size
      indices
      (count indices)
    )
  )
)

(defn generate_grid [size, grid_type]
  (case grid_type
    :square (grid size)
    :polar (polar_grid size)
    :default (grid size)
  )
)
