(ns mazicus.core
  (:gen-class))

(use 'clojure.pprint)
(use 'common)
(use 'simple)
(use 'binmaze)
(use 'sidewinder)

(ns quil-intro
  (:require [quil.core :as q]))

(defn updateConnection [connection x y value]
  (
   into (sorted-map) (merge
      (into {} (filter (fn [[k v]] (not= k y)) connection))
      {y (assoc (get connection y) x value)}
    )
  )
)


(defn draw []
  (q/background 255)
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
   (doseq [t (range 0 100 0.01)]
     (q/point (* t (q/sin t))
              (* t (q/cos t))))))

(q/defsketch trigonometry
  :size [300 300]
  :draw draw
)

(defn -main [ ] 
  (sketch :title "your title" :size [your-width your-height] :setup setup :draw draw)
)
