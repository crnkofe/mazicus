(ns graph)

(use 'common)
(use '[clojure.set :only (difference, union)])


(defn contains_node [node, neighbours]
  (some #(= % [(:x node) (:y node)]) neighbours)
)

(defn actual_neighbours [node, maze]
  (let [linked_neighbours (map #(point (get % 0) (get % 1) maze) (:neighbours node))
        reverse_linked_neighbours (filter #(contains_node node (:neighbours %)) (reduce concat (map vals (vals maze))))]
    (into [] (union (into #{} linked_neighbours) (into #{} reverse_linked_neighbours)))
  )
)

(defn dijkstra_update_distance [distances, from, node]
  (if (contains? distances node)
    (if (< (+ (get distances from 0) 1) (get distances node))
      (assoc distances node (+ (get distances from 0) 1))
      distances
    )
    (assoc distances node (+ (get distances from 0) 1))
  )
)

(defn dijkstra_update_distances [distances, from, nodes]
  (if (not (empty? nodes))
    (loop [node (first nodes)
           remaining (rest nodes)
           current_distances distances]
      (let [updated_distances (dijkstra_update_distance current_distances from node)]
        (if (not (empty? remaining))
          (recur (first remaining) (rest remaining) updated_distances)
          updated_distances
        )
      )
    )
    distances
  )
)

(defn dijkstra [maze] 
  (let [all_nodes (reduce concat (map vals (vals maze)))
        start_node (rand-nth all_nodes)]
    (loop [node start_node
           unvisited [start_node]
           visited #{}
           distances {start_node 0}]
      (if (> (count unvisited) 0)
        (let [neighbours (into [] (difference (into #{} (actual_neighbours node maze)) visited))
              updated_distances (dijkstra_update_distances distances node neighbours)
              new_unvisited (filter #(not= % node) (concat unvisited neighbours))]
          (if (empty? new_unvisited)
            (let [remaining (into [] (difference (difference (into #{} all_nodes) (into #{} new_unvisited)) visited))]
              updated_distances
            )
            (recur (first new_unvisited) new_unvisited (conj visited node) updated_distances)
          )
        )
        distances
      )
    )
  )
)
