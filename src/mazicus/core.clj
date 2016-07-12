(ns mazicus.core
  (:gen-class))

(use 'clojure.pprint)

(declare generate_neighbours)

(defrecord Cell [x,y,neighbours])


(defn generate_row [idx, size] 
  (
   into 
   (sorted-map) 
   (
    reduce conj (map #(hash-map % (->Cell % idx (generate_neighbours % idx size))) (range size))
   )
  )
)

(defn is_valid_cell [x, y, size]
  (
    cond 
      (< x 0) false
      (>= x size) false
      (< y 0) false
      (>= y size) false
      :else true
  )
)

(defn generate_neighbours [x, y, size] 
  (filter 
   #(is_valid_cell (get % 0) (get % 1) size) 
   [
    [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]
   ])
)

(defn connection [size]
  (into (sorted-map) (reduce conj (map #(hash-map % (generate_row % size)) (range size))))
)

(defn updateConnection [connection x y value]
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
