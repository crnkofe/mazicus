(ns sidewinder)

(use 'common)
(use 'clojure.pprint)
(use '[clojure.set :only (difference)])

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
        north_side (not (is_valid_cell (:x last_node) (inc (:y last_node)) (:size graph)))
        right_visited (> 0 (count (filter #(and (= (:x %) (inc (:x last_node))) (= (:y %) (:y last_node))) visited)))]
    (if (or north_side (and (not right_visited) (= coin_flip :carve_east)))
      (if (is_valid_cell (inc (:x last_node)) (:y last_node) (:size graph))
        (let []
         [(conj path (point [(:y last_node) (inc (:x last_node))] graph))
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
  (let [updated_visited (into [] (distinct (concat visited path)))
        all_indices (map coords (all_nodes graph))
        differ (into [] (difference (into #{} all_indices) (into #{} (map coords updated_visited))))]
    (if (empty? differ)
      [nil updated_visited]
      [(first (sort-by #(+ (get % 0) (* 10 (get % 1))) differ)) updated_visited]
    )
  )
)  

(defn generate_sidewinder_path [size]
  (let [initial_maze (grid size)
        initial_cell (point [0 0] initial_maze)
        initial_path [initial_cell]]
    (loop [current_path initial_path
           updated_maze initial_maze
           current_visited [initial_cell]]
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
