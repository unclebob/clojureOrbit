(ns physics.vector (:refer-clojure :exclude (vector)))

(defstruct vector :x :y)

(defn zero_mag? [v]
  (and (zero? (:x v)) (zero? (:y v)))
  )

(defn make
  ([]
    (struct vector 0 0))
  ([x y]
    (struct vector x y))
  )

(defn add [v1 v2]
  (make
    (+ (:x v1) (:x v2))
    (+ (:y v1) (:y v2)))
  )

(defn subtract [v1 v2]
  (make
    (- (:x v1) (:x v2))
    (- (:y v1) (:y v2)))
  )

(defn scale [v s]
  (make
    (* (:x v) s)
    (* (:y v) s))
  )

(defn magnitude [v]
  (letfn [(square [x] (* x x))]
    (Math/sqrt
      (+
        (square (:x v))
        (square (:y v))))
    )
  )

(defn unit [v]
  (scale v (/ 1 (magnitude v))))

(defn rotate90 [{x :x y :y}]
  (make (- y) x))
