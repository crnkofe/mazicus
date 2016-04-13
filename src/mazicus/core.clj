(ns mazicus.core
  (:gen-class))

(deftype Cell [x,y])


(defn innerRow [size] (reduce conj (map #(hash-map % :noConnection) (range size))))

(defn connection [size]
  (reduce conj (map #(hash-map % (innerRow size)) (range size)))
)

(defn updateConnectionRow [row x value]
  (conj row {x value})
)

(defn updateConnection [connection x y value]
  (conj 
    (reduce hash-map (reduce conj (filter (fn [[k v]] (not= k y)) connection)))
    (reduce hash-map (
                      updateConnectionRow (reduce conj (filter (fn [[k v]] (= k y)) connection)) x value
                     )
    )
  )
)

(defn -main
  [& args]
  (print (connection 5))
  )
