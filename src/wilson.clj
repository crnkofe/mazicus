(ns wilson)

(use 'common)
(use '[clojure.set :only (difference)])

(defn valid_path_nodes [maze, visited, path]
  (into [](difference (difference (into #{} (map #(vector (:x %) (:y %)) (all_nodes maze))) (into #{} visited)) (into #{} path)))
)

(defn wilson_carve_path [maze, path]
  (loop [path_node (first path)
         path_remaining (rest path)
         updated_maze maze]
    (if (empty? path_remaining)
      updated_maze
      (let [p1 (point (get path_node 0) (get path_node 1) maze)
            p2 (point (get (first path_remaining) 0) (get (first path_remaining) 1) maze)
            new_maze (connect_nodes p1 p2 updated_maze)]
        (recur (first path_remaining) (rest path_remaining) new_maze)
      )
    )
  )
)

(defn wilson_loop_erased_path [maze, visited, size]
  (let [initial_node (rand-nth (valid_path_nodes maze visited []))]
    (loop [node initial_node
           path [node]]
      (if (some #(= node %) visited)
        path
        (let [next_node (rand-nth (generate_neighbours (get node 0) (get node 1) size))]
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
        initial_cell (get-in initial_maze [(rand-int size) (rand-int size)])]
    (loop [updated_maze initial_maze
           current_visited [[(:x initial_cell) (:y initial_cell)]]]
      (if-not (= (count current_visited) (* size size))
        (let [new_path (wilson_loop_erased_path updated_maze current_visited size)
              new_visited (concat current_visited (butlast new_path))
              new_maze (wilson_carve_path updated_maze new_path)]
          (recur new_maze new_visited)
        )
        updated_maze
      )
    )
  )
)
