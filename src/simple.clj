(ns simple)

(use 'common)
(use 'clojure.pprint)

(defn generate_ne[x, y, size]
  (filter 
   #(is_valid_cell (get % 0) (get % 1) size) 
   [
    [(inc x) y] [x (inc y)]
   ])
)

(defn carve_wall [from_list, visited, graph, size]
  (println "mega")
  (println from_list)
  (if (not (empty? from_list))
    (let [shuffle_from (shuffle from_list)
          rnd_from (first shuffle_from)
          neighbours (generate_ne (:x rnd_from) (:y rnd_from) size)
          ]
      (if (not (empty? neighbours))
        (comment "kle morm popravt ta del k zamenja vsebino celice")
        (let [ne_neighbour (rand-nth neighbours)
              filtered_from (filter #((and (= (:x rnd_from) (:x %)) (= (:y rnd_from) (:y %)))) from_list)]
          (carve_wall (rest shuffle_from) (conj visited rnd_from) graph size)
        )
        (carve_wall (rest shuffle_from) (conj visited rnd_from) graph size)
      )
    )
    :default
  )
)


(defn carve_wall1 [from_list, visited, graph, size]
  (let [from (rand-nth from_list)
        ne_neighbour (rand-nth (generate_ne (:x from) (:y from) size))]
    (if (not (empty? from_list))
      (let [filtered_from (filter #((and (= (:x from) (:x %)) (= (:y from) (:y %)))) from_list)]
        (println "asdasd")
        (println from)
        (println (conj visited from))
        (assoc (get (get graph (:y from)) (:x from)) :neighbours ne_neighbour)
        (println "www")
        (println filtered_from)
        (println visited)
        (carve_wall filtered_from visited graph size)
      )
      :default
    )
  )
)

(defn generate_subindices [idx, size] 
  (into [] (reduce concat (map #(vector [idx %]) (range size))))
)

(defn generate_indices [size]
  (into [] (reduce concat (map #(generate_subindices % size) (range size))))
)

(defn carve_binary_maze [size]
  (let [maze (grid size)
        from (generate_indices size)
        ]
    (carve_wall (map #(point (get % 0) (get % 1) maze) from) #{} maze size)
  )
)
