(ns recursivebacktracker)

(use 'common)
(use '[clojure.set :only (difference, union, intersection)])

(defn carve_recursive_backtrack_maze [size]
  (let [initial_maze (grid size)
        initial_cell (get-in initial_maze [(rand-int size) (rand-int size)])]
    (loop [path_stack [initial_cell]
           visited [(coords initial_cell)]
           maze initial_maze]
      (if (empty? path_stack)
        maze
        (let [neighbours (generate_neighbours (:x (last path_stack)) (:y (last path_stack)) size)
              unvisited (into [] (difference (difference (into #{} neighbours) (into #{} (map coords path_stack))) (into #{} visited)))]
            (if (empty? unvisited)
              (let [last_visited (last path_stack)]
                (if (> (count path_stack) 1)
                  (let [before_last_visited (get path_stack (- (count path_stack) 2))
                        updated_maze (connect_nodes last_visited before_last_visited maze)]
                    (recur (pop path_stack) (conj visited (coords last_visited)) updated_maze) 
                  )
                  (recur (pop path_stack) (conj visited (coords last_visited)) maze) 
                )
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
