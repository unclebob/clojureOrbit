(ns physics.position)

(defrecord position [^double x ^double y])

(defn make
  ([]    (position. 0 0))
  ([x y] (position. x y)))

(defn origin? [p]
  (and (zero? (:x p)) (zero? (:y p))))

(defn add [{x1 :x y1 :y} {x2 :x y2 :y}]
  (position. (+ x1 x2) (+ y1 y2)))

(defn subtract [{x1 :x y1 :y} {x2 :x y2 :y}]
  (position. (- x1 x2) (- y1 y2)))

(defn distance [{x1 :x y1 :y} {x2 :x y2 :y}]
  (Math/sqrt
   (+
    (Math/pow (- x1 x2) 2)
    (Math/pow (- y1 y2) 2))))

(defn mean [a b]
  (/ (+ a b) 2))

(defn average [{x1 :x y1 :y} {x2 :x y2 :y}]
  (position. (mean x1 x2) (mean y1 y2)))
