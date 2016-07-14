(ns sidewinder)

(use 'common)
(use 'clojure.pprint)

(defn sidewinder_unused [graph, visited]
  (get-in graph [0 0])
)

(defn carve_top [cell graph]
  (if (is_valid_grid_cell (:x cell) (:y cell) graph)
    (assoc-in graph [(:y cell) (:x cell) :neighbours] (conj (get-in graph [(:y cell) (:x cell) :neighbours]) [(:x cell) (inc (:y cell))]))
    graph
  )
)

(defn carve_right [cell graph]
  (assoc-in graph [(:y cell) (:x cell) :neighbours] (conj (get-in graph [(:y cell) (:x cell) :neighbours]) [(inc (:x cell)) (:y cell)]))
)

(defn sidewinder_close [cell, graph, visited]
  (carve_top cell graph)
)

(defn sidewinder_next [path, graph, visited]
  (let [last_node (first (reverse path))
        coin_flip (rand-nth [:carve_east :join_path])]
    (if (= coin_flip :carve_east)
      (if (is_valid_grid_cell (inc (:x last_node)) (:y last_node) graph)
        (let []
         [(conj path (get-in graph [(:y last_node) (inc (:x last_node))]))
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

(defn generate_sidewinder_path [size]
  (let [initial_maze (grid size)
        initial_cell (get-in initial_maze [0 0])
        initial_path [initial_cell]]
    (loop [current_path initial_path
           updated_maze initial_maze
           current_visited [initial_cell]]
      (let [[path maze] (sidewinder_next current_path updated_maze current_visited)]
        (if-not (= (count current_path) (count path))
          (recur path maze current_path)
          maze
        )
      )
    )
  )
)

(defn str_sidewinder_maze [size]
  (print_grid (generate_sidewinder_path size))
)
