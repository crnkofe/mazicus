(ns graph)

(use 'common)

(defn dijkstra_update_distance [distances, from, node]
  (if (contains? distances node)
    (if (< (get distances node) (get distances from))
      (let [updated_from (assoc distances from (get distances node))]
        (assoc updated_from node (+ (get distances from) 1))
      )
      (assoc distances node (+ (get distances from) 1))
    )
    (assoc distances node (+ (get distances from) 1))
  )
)

(defn dijkstra_update_distances [distances, from, nodes]
  (if (not (empty? nodes))
    (loop [node (first nodes)
           remaining (rest nodes)
           current_distances distances]
      (let [updated_distances (dijkstra_update_distance distances from node)]
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
           distances {start_node 0}]
      (if (> (count unvisited) 0)
        (let [neighbours (map #(point (get % 0) (get % 1) maze) (:neighbours node))
              updated_distances (dijkstra_update_distances distances node neighbours)
              new_unvisited (filter #(not= % node) (conj unvisited neighbours))]
          (recur (first new_unvisited) new_unvisited updated_distances)
        )
        distances
      )
    )
  )
)
