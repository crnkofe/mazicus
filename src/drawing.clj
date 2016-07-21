(ns drawing
  (:require [quil.core :as q])
)

(use 'common)

(declare draw_maze_row)
(declare draw_polar_maze_row)

(defprotocol DrawableMazeProtocol
  (draw [maze, dkstr])
)

(extend-type common.Grid DrawableMazeProtocol
  (draw [maze, dkstr]
    (let [row_keys (reverse (sort (keys (:cells maze))))
          size (count (keys (:cells maze)))
          cell_size {:x (/ 550 size) :y (/ 550 size)}
          initial_row_y 0]
      (loop [current_key (first row_keys)
             row_keys (into [] (rest row_keys))
             row_y initial_row_y]
        (draw_maze_row (get (:cells maze) current_key) row_y cell_size (count (keys (:cells maze))) dkstr)
        (if (not (empty? row_keys))
          (recur (first row_keys) (rest row_keys) (+ row_y (:y cell_size)))
          nil
        )
      )
    )
  )
)

(extend-type common.PolarGrid DrawableMazeProtocol
  (draw [maze, dkstr]
    (let [row_keys (reverse (sort (keys (:cells maze))))
          size (count (keys (:cells maze)))
          radius (/ 550 2)
          cell_size {:length (/ radius size)}
          initial_row_y 0
          center {:x (/ 550 2) :y (/ 550 2)}
          ]
      (loop [current_key (first row_keys)
             row_keys (into [] (rest row_keys))
             row_y initial_row_y]
        (draw_polar_maze_row (get (:cells maze) current_key) row_y cell_size (count (keys (:cells maze))) dkstr center)
        (if (not (empty? row_keys))
          (recur (first row_keys) (rest row_keys) (+ row_y (:length cell_size)))
          nil
        )
      )
    )
  )
)

(defn neighbour [cell, direction, size]
  (case direction
    :left [(dec (:x cell)) (:y cell)]
    :right [(inc (:x cell)) (:y cell)]
    :top [(:x cell) (inc (:y cell))]
    :bottom [(:x cell) (dec (:y cell))]
  )
)

(defn draw_maze_cell [cell, x, y, cell_size, size, distances]
  (let [left (neighbour cell :left size)
        right (neighbour cell :right size)
        top (neighbour cell :top size)
        bottom (neighbour cell :bottom size)
        max_val (apply max (vals distances))
        current_val (get distances cell 0)]
    (if (> current_val 0)
      (let [div_val (/ current_val max_val)]
        (q/fill 255 0 0 (* div_val 255))
        (q/no-stroke)
        (q/rect x y (:x cell_size) (:y cell_size))
      )
    )
    (q/stroke-weight 2)
    (q/stroke 0 0 0)
    (if (not (is_valid_cell (get left 0) (get left 1) size))
      (q/line x y x (+ y (:y cell_size)))
    )
    (if (not (is_valid_cell (get bottom 0) (get bottom 1) size))
      (q/line x (+ y  (:y cell_size)) (+ x (:x cell_size)) (+ y (:y cell_size)))
    )
    (if (not (some #(= % top) (:neighbours cell)))
      (q/line x y (+ x (:x cell_size)) y)
    )
    (if (not (some #(= % right) (:neighbours cell)))
      (q/line (+ x (:x cell_size)) y (+ x (:x cell_size)) (+ y (:y cell_size)))
    )
  )
)

(defn draw_maze_row [row, row_y, cell_size, size, distance]
  (doall (map #(draw_maze_cell % (* (:x %) (:x cell_size)) row_y cell_size size distance) (map #(get row %) (sort (keys row)))))
)

(defn polar_neighbour [cell, direction, size]
  (case direction
    :left [(dec (:x cell)) (:y cell)]
    :right [(inc (:x cell)) (:y cell)]
    :top [(:x cell) (inc (:y cell))]
    :bottom [(:x cell) (dec (:y cell))]
  )
)

(defn draw_polar_maze_cell [cell, cell_size, size, center]
  (let [cell_degrees (/ 360 size)
        rad1 (Math/toRadians (:rad cell)) 
        rad2 (Math/toRadians (+ (:rad cell) cell_degrees))
        right_bot [(+ (:x center) (* (:length cell_size) (Math/cos rad1) (:dist cell))) 
                   (+ (:y center) (* (:length cell_size) (Math/sin rad1) (:dist cell)))]
        right_top [(+ (:x center) (* (:length cell_size) (Math/cos rad1) (inc (:dist cell))))
                   (+ (:y center) (* (:length cell_size) (Math/sin rad1) (inc (:dist cell))))]
        left_bot [(+ (:x center) (* (:length cell_size) (Math/cos rad2) (:dist cell)))
                  (+ (:y center) (* (:length cell_size) (Math/sin rad2) (:dist cell)))]
        left_top [(+ (:x center) (* (:length cell_size) (Math/cos rad2) (inc (:dist cell))))
                  (+ (:y center) (* (:length cell_size) (Math/sin rad2) (inc (:dist cell))))]]
    (q/line right_bot right_top)
    (q/line left_bot left_top)
    (q/line right_top left_top)
  )
)

(defn draw_polar_maze_row [row, row_y, cell_size, size, distance, center]
  (doall (map #(draw_polar_maze_cell % cell_size size center) (map #(get row %) (sort (keys row)))))
)
