(ns sidewinder)

(use 'common)
(use 'clojure.pprint)
(use '[clojure.set :only (difference, union)])

(defn carve_top [cell graph]
  (if (is_valid_cell [(:x cell) (inc (:y cell))] (:size graph))
    (connect_nodes cell (point [(:x cell) (inc (:y cell))] graph) graph)
    graph
  )
)

(defn carve_right [cell graph]
  (if (is_valid_cell [(inc (:x cell)) (:y cell)] (:size graph))
    (connect_nodes cell (point [(inc (:x cell)) (:y cell)] graph) graph)
  )
)

(defn sidewinder_close [cell, graph, visited]
  (carve_top cell graph)
)

(defn sidewinder_next [path, graph, visited]
  (let [last_node (first (reverse path))
        coin_flip (rand-nth [:carve_east :join_path])
        no_north_side (not (is_valid_cell (:x last_node) (inc (:y last_node)) (:size graph)))
        no_right_side (not (is_valid_cell (inc (:x last_node)) (:y last_node) (:size graph)))]
    (if (and (not no_right_side) (or no_north_side  (= coin_flip :carve_east)))
      (if (is_valid_cell (inc (:x last_node)) (:y last_node) (:size graph))
        (let []
         [(conj path (point [(inc (:x last_node)) (:y last_node) ] graph))
          (carve_right last_node graph)]
        )
        [path (carve_top last_node graph)]
      )
      (let [sidewinder_cell (rand-nth path)]
         [path (carve_top sidewinder_cell graph)]
      )
    )
  )
)

(defn sidewinder_path [path, graph, visited]
  (let [updated_visited (union visited (into #{} path))
        differ (into [] (difference (:all_indices graph) (into #{} (map coords updated_visited))))]
    (if (empty? differ)
      [nil updated_visited]
      (let [min_y (apply max (map #(get % 1) differ))
            min_differ_cells (filter #(= min_y (get % 1)) differ) 
            min_y_cells (sort-by #(get % 0) min_differ_cells)]
        [(first min_y_cells) updated_visited]
      )
    )
  )
)  

(defn generate_sidewinder_path [size]
  (let [initial_maze (grid size)
        initial_cell (point [0 (dec size)] initial_maze)
        initial_path [initial_cell]]
    (loop [current_path initial_path
           updated_maze initial_maze
           current_visited #{initial_cell}]
      (let [[path maze] (sidewinder_next current_path updated_maze current_visited)]
        (if-not (= (count current_path) (count path))
          (recur path maze current_visited)
          (let [[path_start new_visited] (sidewinder_path path maze current_visited)]
            (if (= nil path_start)
              maze
              (recur [(point path_start maze)] maze new_visited)
            )
          )
        )
      )
    )
  )
)

(defn str_sidewinder_maze [size]
  (print_grid (generate_sidewinder_path size))
)
