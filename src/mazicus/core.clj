(ns mazicus.core
  (:gen-class))

(use 'clojure.pprint)
(use 'common)
(use 'simple)

(defn updateConnection [connection x y value]
  (
   into (sorted-map) (merge
      (into {} (filter (fn [[k v]] (not= k y)) connection))
      {y (assoc (get connection y) x value)}
    )
  )
)

(defn visit_graph_next [node, visited, graph, depth]
  (if (< depth 10)
    (let [visited_new (conj visited [(:x node) (:y node)])]
      (map #(visit_graph_next (point (get % 0) (get % 1) graph) visited_new graph (inc depth)) (filter_visited visited_new (:neighbours node)) )
    )
    :default
  )
)

(comment "All depth-first maze traversals from point")
(defn visit_graph [graph] 
  (visit_graph_next (point 0 0 graph) #{} graph 0)
)

(defn visit_graph_breadth_next [priority, visited, graph]
  (let [node (first priority)
        visited_new (conj visited [(:x node) (:y node)])]
    (if (not node) :default
      (let [priority_new (into [] (concat (rest priority) (map #(point (get % 0) (get % 1) graph) (filter_visited visited_new (:neighbours node)))))]
        (visit_graph_breadth_next priority_new visited_new graph))
    )
  )
  :default
)

(comment "All breadth-first maze traversals from point")
(defn visit_graph_breadth [graph] 
  (visit_graph_breadth_next [(point 0 0 graph)] #{} graph)
)

(defn -main
  [& args]
  (pprint (connection 5))
)
