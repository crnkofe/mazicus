(ns aldousbroder)

(use 'common)

(defn carve_aldbro_maze [size, maze_type]
  (let [initial_maze (generate_grid size maze_type)
        initial_cell (random_point initial_maze)]
    (loop [current_cell initial_cell
           updated_maze initial_maze
           current_visited [(coords initial_cell)]]
      (if-not (= (count current_visited) (:count updated_maze))
        (let [neighbour_coords (rand-nth (:valid_neighbours current_cell))
              neighbour (point neighbour_coords updated_maze)]
          (if (not (some #(= neighbour_coords %) current_visited))
            (let [new_maze (connect_nodes current_cell neighbour updated_maze)]
              (recur neighbour new_maze (conj current_visited neighbour_coords))
            )
            (recur neighbour updated_maze current_visited)
          )
        )
        updated_maze
      )
    )
  )
)
