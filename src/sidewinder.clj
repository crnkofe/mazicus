(ns sidewinder)

(use 'common)
(use 'clojure.pprint)
(use '[clojure.set :only (difference, union)])

(declare carve_right)

(defn carve_top [cell graph]
  (let [top_neighbours (neighbours_direction cell :north)
        top_neighbour (first top_neighbours)]
    (if (not= top_neighbour nil)
      (if (within_bounds graph top_neighbour)
        (let [updated_graph (connect_nodes cell (point top_neighbour graph) graph)]
          updated_graph
        )
        graph
      )
      graph
    )
  )
)

(defn carve_right [cell graph]
  (let [right_neighbours (neighbours_direction cell :east)
        right_neighbour (first right_neighbours)]
    (if (and (not (= right_neighbour nil)) (within_bounds graph right_neighbour))
      (connect_nodes cell (point right_neighbour graph) graph)
      graph
    )
  )
)

(defn sidewinder_close [cell, graph, visited]
  (carve_top cell graph)
)

(defn sidewinder_next [path, graph, visited]
  (let [last_node (first (reverse path))
        coin_flip (rand-nth [:carve_east :join_path])
        north_neighbour (first (neighbours_direction last_node :north))]
    (if (or (= north_neighbour nil) (= coin_flip :carve_east))
      (let [east_node (first (neighbours_direction last_node :east))]
        (if (and (not (= nil east_node)) (within_bounds graph east_node))
          [(conj path (point east_node graph)) (carve_right last_node graph)]
          [path (carve_top last_node graph)]
        )
      )
      [path (carve_top (rand-nth path) graph)]
    )
  )
)

(defn sidewinder_path [path, graph, visited, grid_type]
  (let [updated_visited (union visited (into #{} path))
        differ (into [] (difference (:all_indices graph) (into #{} (map coords updated_visited))))]
    (if (empty? differ)
      [nil updated_visited]
      (let [min_y (apply max (map #(get % 1) differ))
            min_differ_cells (filter #(= min_y (get % 1)) differ) 
            min_y_cells (sort-by #(get % 0) min_differ_cells)]
        (if (= grid_type :square)
          [(first min_y_cells) updated_visited]
          [(first (reverse min_y_cells)) updated_visited]
        )
      )
    )
  )
)  

(defn generate_sidewinder_path [size, grid_type]
  (let [initial_maze (generate_grid size grid_type)
        initial_cell (point [0 0] initial_maze)
        initial_path [initial_cell]]
    (loop [current_path initial_path
           updated_maze initial_maze
           current_visited #{initial_cell}]
      (let [[path maze] (sidewinder_next current_path updated_maze current_visited)]
        (if-not (= (count current_path) (count path))
          (recur path maze current_visited)
          (let [[path_start new_visited] (sidewinder_path path maze current_visited grid_type)]
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
