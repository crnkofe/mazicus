(ns wilson)

(use 'common)
(use '[clojure.set :only (difference, union)])

(defn valid_path_nodes [maze, visited, path]
  (into [] (difference (difference (:all_indices maze) visited) (into #{} path)))
)

(defn wilson_carve_path [maze, path]
  (loop [path_node (first path)
         path_remaining (rest path)
         updated_maze maze]
    (if (empty? path_remaining)
      updated_maze
      (let [p1 (point path_node maze)
            p2 (point (first path_remaining) maze)
            new_maze (connect_nodes p1 p2 updated_maze)]
        (recur (first path_remaining) (rest path_remaining) new_maze)
      )
    )
  )
)

(defn wilson_loop_erased_path [maze, visited]
  (let [initial_node (rand-nth (valid_path_nodes maze visited []))]
    (loop [node initial_node
           path [node]]
      (if (some #(= node %) visited)
        path
        (let [next_node (rand-nth (generate_neighbours node (:size maze)))]
          (if (some #(= next_node %) path)
            (let [new_start_node (rand-nth (valid_path_nodes maze visited []))]
              (recur new_start_node [new_start_node])
            )
            (recur next_node (conj path next_node))
          )
        )
      )
    )
  )
)

(defn carve_wilson_maze [size]
  (let [initial_maze (grid size)
        initial_cell (random_point initial_maze)]
    (loop [updated_maze initial_maze
           current_visited #{(coords initial_cell)}]
      (if-not (= (count current_visited) (* size size))
        (let [new_path (wilson_loop_erased_path updated_maze current_visited)
              new_visited (union current_visited (into #{} (butlast new_path)))
              new_maze (wilson_carve_path updated_maze new_path)]
          (recur new_maze new_visited)
        )
        updated_maze
      )
    )
  )
)
