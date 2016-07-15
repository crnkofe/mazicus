(ns mazicus.core
  (:gen-class)
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:require [quil.core :as q])
)

(use 'clojure.pprint)
(use 'common)
(use 'simple)
(use 'binmaze)
(use 'sidewinder)

(defn draw []
  (q/background 255)
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
   (doseq [t (range 0 100 0.01)]
     (q/point (* t (q/sin t))
              (* t (q/cos t))))))

(q/defsketch trigonometry
  :size [640 480]
  :draw draw
)

(defn setup []
  (q/frame-rate 1)
  (q/background 200)
)

(defn -main [& args] 
  (q/sketch :title "Mazicus!" :size [640 480] :setup setup :draw draw)
)
