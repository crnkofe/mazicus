(ns huntnkill)

(use 'common)
(use '[clojure.set :only (difference, union)])

(comment
(defn neighbour_or_hunt [from, maze, visited]

)
)

(defn valid_neighbours [maze, from, visited, size]
  (println from visited size)
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
              (recur next_cell (connect_nodes current_cell next_cell updated_maze) (conj current_visited next))
            )
            (let []
              (comment "hunt&kill")
              updated_maze
            )
          )
        )
        updated_maze
      )
    )
  )
)
