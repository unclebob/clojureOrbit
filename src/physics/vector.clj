(ns physics.vector
  (:refer-clojure :exclude (vector))
  (:require [physics.position :as position])
  (:import physics.position.position))


(defn make
  ([]    (position. 0 0))
  ([x y] (position. x y)))

(defn zero_mag? [v]
  (position/origin? v))

(def add position/add)
(def subtract position/subtract)

(defn scale [v s]
  (make
   (* (:x v) s)
   (* (:y v) s)))

(defn magnitude [v]
  (Math/sqrt
   (+
    (Math/pow (:x v) 2)
    (Math/pow (:y v) 2))))

(defn unit [v]
  (scale v (/ 1 (magnitude v))))

(defn rotate90 [{x :x y :y}]
  (make (- y) x))

(defn equal [{x1 :x y1 :y} {x2 :x y2 :y}]
  (and (== x1 x2) (== y1 y2)))