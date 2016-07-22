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
    (if (not (is_valid_cell left size))
      (q/line x y x (+ y (:y cell_size)))
    )
    (if (not (is_valid_cell bottom size))
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
  (let [radial_move (/ 360 size)]
    (case direction
      :left [(+ (:rad cell) radial_move) (:dist cell)]
      :right [(- (:rad cell) radial_move) (:dist cell)]
      :top [(:rad cell) (inc (:dist cell))]
      :bottom [(:rad cell) (dec (:dist cell))]
    )
  )
)

(defn scos [degrees] 
  (if (and (> degrees 90) (< degrees 270))
    (* -1 (Math/cos degrees))
    (Math/cos degrees)
  )
)

(defn ssin [degrees] 
  (if (and (> degrees 180) (< degrees 360))
    (* -1 (Math/sin degrees))
    (Math/sin degrees)
  )
)

(defn draw_polar_maze_cell [cell, cell_size, size, center, distances]
  (let [cell_degrees (/ 360 size)
        rad1 (Math/toRadians (:rad cell)) 
        rad2 (Math/toRadians (+ (:rad cell) cell_degrees))
        right_bot [(+ (:x center) (* (:length cell_size) (scos rad1) (:dist cell))) 
                   (+ (:y center) (* (:length cell_size) (ssin rad1) (:dist cell)))]
        right_top [(+ (:x center) (* (:length cell_size) (scos rad1) (inc (:dist cell))))
                   (+ (:y center) (* (:length cell_size) (ssin rad1) (inc (:dist cell))))]
        left_bot [(+ (:x center) (* (:length cell_size) (scos rad2) (:dist cell)))
                  (+ (:y center) (* (:length cell_size) (ssin rad2) (:dist cell)))]
        left_top [(+ (:x center) (* (:length cell_size) (scos rad2) (inc (:dist cell))))
                  (+ (:y center) (* (:length cell_size) (ssin rad2) (inc (:dist cell))))]
        left (polar_neighbour cell :left size)
        right (polar_neighbour cell :right size)
        top (polar_neighbour cell :top size)
        bottom (polar_neighbour cell :bottom size)
        max_val (apply max (vals distances))
        current_val (get distances cell 0)]
    (if (> current_val 0)
      (let [div_val (/ current_val max_val)]
        (q/fill 255 0 0 (* div_val 255))
        (q/no-stroke)
        (q/quad (get right_top 0) (get right_top 1)
                (get left_top 0) (get left_top 1)
                (get left_bot 0) (get left_bot 1)
                (get right_bot 0) (get right_bot 1)
        )
      )
    )
    (q/stroke-weight 2)
    (q/stroke 0 0 0)
    (if (not (is_valid_polar_cell left size))
      (q/line left_bot left_top)
    )
    (if (not (is_valid_polar_cell bottom size))
      (q/line left_bot right_bot)
    )
    (if (not (some #(= % top) (:neighbours cell)))
      (q/line right_top left_top)
    )
    (if (not (some #(= % right) (:neighbours cell)))
      (q/line right_top right_bot)
    )
    (if (not (is_valid_polar_cell right size))
      (q/line right_top right_bot)
    )
  )
)

(defn draw_polar_maze_row [row, row_y, cell_size, size, distances, center]
  (doall (map #(draw_polar_maze_cell % cell_size size center distances) (map #(get row %) (sort (keys row)))))
)
