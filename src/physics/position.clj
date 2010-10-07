(ns physics.position)

(defn origin? [p]
  (every? zero? p))

(defn make
  ([]    [0 0])
  ([x y] [x y]))

(defn add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn subtract [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn distance [[x1 y1] [x2 y2]]
  (Math/sqrt
   (+
    (Math/pow (- x1 x2) 2)
    (Math/pow (- y1 y2) 2))))

(defn mean [a b]
  (/ (+ a b) 2))

(defn average [[x1 y1] [x2 y2]]
  (make (mean x1 x2) (mean y1 y2)))
