(ns huntnkill)

(use 'common)
(use '[clojure.set :only (difference, union, intersection)])

(defn filter_w_visited [index_node, maze, size, visited]
  (let [neighbours (generate_neighbours (get index_node 0) (get index_node 1) size)
        visited_neighbours (into [] (intersection (into #{} neighbours) (into #{} visited)))]
    (> (count visited_neighbours) 0)
  )
)

(defn hunt [from, maze, size, visited]
  (let [row_keys (reverse (keys maze))]
    (loop [current_keys row_keys]
      (if (empty? current_keys)
        nil
        (let [row_indices (into [] (map #(vector % (first current_keys)) (range size)))
              non_visited_row_indices (into [] (difference (into #{} row_indices) (into #{} visited)))
              filtered_visited (filter #(filter_w_visited % maze size visited) non_visited_row_indices)]
          (if (not (empty? filtered_visited))
            (first filtered_visited)
            (recur (rest current_keys))
          )
        )
      )
    )
  )
)

(defn valid_neighbours [maze, from, visited, size]
  (into [] (difference (into #{} (generate_neighbours (:x from) (:y from) size)) (into #{} visited)))
)

(defn carve_huntnkill_maze [size]
  (let [initial_maze (grid size)
        initial_cell (get-in initial_maze [(rand-int size) (rand-int size)])]
    (loop [current_cell initial_cell
           updated_maze initial_maze
           current_visited [[(:x initial_cell) (:y initial_cell)]]]
      (if-not (= (count current_visited) (* size size))
        (let [unvisited_neighbour_coords (valid_neighbours updated_maze current_cell current_visited size)]
          (if (not (empty? unvisited_neighbour_coords))
            (let [next_coords (rand-nth unvisited_neighbour_coords)
                  next_cell (point (get next_coords 0) (get next_coords 1) updated_maze)]
              (recur next_cell (connect_nodes current_cell next_cell updated_maze) (conj current_visited next_coords))
            )
            (let [hunted_coords (hunt current_cell updated_maze size current_visited)
                  hunted (point (get hunted_coords 0) (get hunted_coords 1) updated_maze)]
              (if (not (= hunted nil))
                (let [neighbours (generate_neighbours (:x hunted) (:y hunted) size)
                      visited_neighbours (into [] (intersection (into #{} neighbours) (into #{} current_visited)))
                      random_from_coords (rand-nth visited_neighbours)
                      random_from (point (get random_from_coords 0) (get random_from_coords 1) updated_maze)]
                    (recur hunted (connect_nodes random_from hunted updated_maze) (conj current_visited hunted_coords))
                )
                updated_maze
              )
            )
          )
        )
        updated_maze
      )
    )
  )
)
