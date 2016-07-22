(ns recursivebacktracker)

(use 'common)
(use '[clojure.set :only (difference, union, intersection)])

(defn carve_backtrack [path_stack, maze]
  (let [last_visited (last path_stack)]
    (if (> (count path_stack) 1)
      (let [before_last_visited (get path_stack (- (count path_stack) 2))
            updated_maze (connect_nodes last_visited before_last_visited maze)]
        updated_maze
      )
      maze
    )
  )
)

(defn carve_recursive_backtrack_maze [size, maze_type]
  (let [initial_maze (generate_grid size maze_type)
        initial_cell (random_point initial_maze)]
    (loop [path_stack [initial_cell]
           visited #{(coords initial_cell)}
           maze initial_maze]
      (if (empty? path_stack)
        maze
        (let [neighbours (:valid_neighbours (last path_stack))
              unvisited (into [] (difference (difference (into #{} neighbours) (into #{} (map coords path_stack))) visited))]
            (if (empty? unvisited)
              (let [updated_maze (carve_backtrack path_stack maze)
                    last_visited (last path_stack)]
                (recur (pop path_stack) (conj visited (coords last_visited)) updated_maze) 
              )
              (let [next_node (rand-nth unvisited)]
                (recur (conj path_stack (point next_node maze)) visited maze)
              )
            )
        )
      )
    )
  )
)
