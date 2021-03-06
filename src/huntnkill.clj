(ns huntnkill)

(use 'common)
(use '[clojure.set :only (difference, union, intersection)])

(defn filter_w_visited [index_node, maze, visited]
  (let [neighbours (:valid_neighbours (point index_node maze))
        visited_neighbours (into [] (intersection (into #{} neighbours) visited))]
    (> (count visited_neighbours) 0)
  )
)

(defn hunt [from, maze, visited]
  (let [row_keys (reverse (keys (:cells maze)))]
    (loop [current_keys row_keys]
      (if (empty? current_keys)
        nil
        (let [row_indices (into [] (map #(vector % (first current_keys)) (range (:size maze))))
              non_visited_row_indices (into [] (difference (into #{} row_indices) visited))
              filtered_visited (filter #(filter_w_visited % maze visited) non_visited_row_indices)]
          (if (not (empty? filtered_visited))
            (first filtered_visited)
            (recur (rest current_keys))
          )
        )
      )
    )
  )
)

(defn valid_neighbours [maze, from, visited]
  (into [] (difference (into #{} (:valid_neighbours from)) visited))
)

(defn carve_huntnkill_maze [size, maze_type]
  (let [initial_maze (generate_grid size maze_type)
        initial_cell (random_point initial_maze)]
    (loop [current_cell initial_cell
           updated_maze initial_maze
           current_visited #{(coords initial_cell)}]
      (if-not (= (count current_visited) (* size size))
        (let [unvisited_neighbour_coords (valid_neighbours updated_maze current_cell current_visited)]
          (if (not (empty? unvisited_neighbour_coords))
            (let [next_coords (rand-nth unvisited_neighbour_coords)
                  next_cell (point next_coords updated_maze)]
              (recur next_cell (connect_nodes current_cell next_cell updated_maze) (conj current_visited next_coords))
            )
            (let [hunted_coords (hunt current_cell updated_maze current_visited)
                  hunted (point hunted_coords updated_maze)]
              (if (not (= hunted nil))
                (let [neighbours (:valid_neighbours hunted)
                      visited_neighbours (into [] (intersection (into #{} neighbours) current_visited))
                      random_from_coords (rand-nth visited_neighbours)
                      random_from (point random_from_coords updated_maze)]
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
