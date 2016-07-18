(ns common)
(use 'clojure.pprint)
(use '[clojure.string :only (join split blank?)])

(declare generate_neighbours)

(defrecord Cell [x,y,neighbours])

(defn point [x, y, graph] 
  (get (get graph y) x)
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

(defn is_valid_cell [[x, y], size]
  (is_valid_cell x y size)
)

(defn is_valid_cell [x, y, size]
  (
    cond 
      (< x 0) false
      (>= x size) false
      (< y 0) false
      (>= y size) false
      :else true
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

(defn generate_neighbours [x, y, size]
  (filter 
   #(is_valid_cell (get % 0) (get % 1) size) 
   [
    [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]
   ])
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
  (into (sorted-map) (reduce conj (map #(hash-map % (generate_grid_row % size)) (range size))))
)
