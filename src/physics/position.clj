(ns physics.position)

(defstruct position :x :y)

(defn origin? [p]
  (and
    (zero? (:x p))
    (zero? (:y p)))
  )

(defn make
  ([]
    (struct position 0 0))
  ([x y]
    (struct position x y))
  )

(defn add [p q]
  (struct position
    (+ (:x p) (:x q))
    (+ (:y p) (:y q)))
  )

(defn subtract [p q]
  (struct position
    (- (:x p) (:x q))
    (- (:y p) (:y q)))
  )

(defn distance [p q]
  (letfn [(square [x] (* x x))]
    (Math/sqrt
      (+
        (square (- (:x p) (:x q)))
        (square (- (:y p) (:y q)))))
    )
  )

(defn average [{x1 :x y1 :y} {x2 :x y2 :y}]
  (letfn [
    (mean [a b] (/ (+ a b) 2))
    ]
    (make (mean x1 x2) (mean y1 y2))
    )
  )
