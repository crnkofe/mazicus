(ns aldousbroder)

(use 'common)

(defn carve_aldbro_maze [size]
  (let [initial_maze (grid size)
        initial_cell (get-in initial_maze [0 0])]
    (loop [current_cell initial_cell
           updated_maze initial_maze
           current_visited [[(:x initial_cell) (:y initial_cell)]]]
      (if-not (= (count current_visited) (* size size))
        (let [neighbour_coords (rand-nth (generate_neighbours (:x current_cell) (:y current_cell) size))
              neighbour (point (get neighbour_coords 0) (get neighbour_coords 1) updated_maze)]
          (if (not (some #(= neighbour_coords %) current_visited))
            (let [idx [(:y current_cell) (:x current_cell) :neighbours]
                  new_maze (assoc-in updated_maze idx (conj (get-in updated_maze idx) neighbour_coords))]
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
