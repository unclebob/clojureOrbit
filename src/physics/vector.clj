(ns physics.vector (:refer-clojure :exclude (vector)))

(defstruct vector :x :y)


(defn unit [v]
  (scale v (/ 1 (magnitude v))))

(defn rotate90 [{x :x y :y}]
  (make (- y) x))
