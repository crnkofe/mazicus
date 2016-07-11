(ns mazicus.core
  (:gen-class))

(use 'clojure.pprint)

(deftype Cell [x,y])


(defn innerRow [size] (into (sorted-map) (reduce conj (map #(hash-map % :notc) (range size)))))

(defn connection [size]
  (into (sorted-map) (reduce conj (map #(hash-map % (innerRow size)) (range size))))
)

(defn updateConnection1 [connection x y value]
  (into {} (filter (fn [[k v]] (not= k y)) connection))
)

(defn updateConnection2 [connection x y value]
  (
   into (sorted-map) (merge
      (into {} (filter (fn [[k v]] (not= k y)) connection))
      {y (assoc (get connection y) x value)}
    )
  )
)


(defn -main
  [& args]
  (pprint (connection 5))
  )
