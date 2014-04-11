(ns physics.vector (:refer-clojure :exclude (vector)))

(defn zero_mag? [v]
  (every? zero? v))

(defn make
  ([]    [0 0])
  ([x y] [x y]))

(defn add [v1 v2]
  (make
    (+ (first v1) (first v2))
    (+ (last v1) (last v2))))

(defn subtract [v1 v2]
  (make
   (- (first v1) (first v2))
   (- (last v1) (last v2))))

(defn scale [v s]
  (make
   (* (first v) s)
   (* (last v) s)))

(defn magnitude [v]
  (Math/sqrt
   (+
    (Math/pow (first v) 2)
    (Math/pow (last v) 2))))

(defn unit [v]
  (scale v (/ 1 (magnitude v))))

(defn rotate90 [[x y]]
  (make (- y) x))

(defn equal [[x1 y1] [x2 y2]]
  (and (== x1 x2) (== y1 y2)))