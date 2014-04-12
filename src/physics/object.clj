(ns physics.object
  (:refer-clojure :exclude (merge))
  (:require [physics.position :as position]
            [physics.vector :as vector]))

(defstruct object :position :mass :velocity :force :name)

(defn make
  ([]
   (struct object (position/make) 0 (vector/make) (vector/make) "TILT"))
  ([position mass velocity force name]
   (struct object position mass velocity force name)))

(defn gravity [m1 m2 r]
  (/ (* m1 m2) (* r r)))

(defn force-between [o1 o2]
  (let [p1 (:position o1)
        p2 (:position o2)
        d (position/distance p1 p2)
        uv (vector/unit (vector/subtract p2 p1))
        g (gravity (:mass o1) (:mass o2) d)]
    (vector/scale uv g)))

(defn accumulate-forces
  ([o world]
   (assoc o :force (accumulate-forces o world (vector/make))))
  ([o world f]
   (cond
     (empty? world) f
     (= o (first world)) (recur o (rest world) f)
     :else (recur o (rest world) (vector/add f (force-between o (first world)))))))

(defn calculate-forces-on-all [world]
  (map #(accumulate-forces % world) world))

(defn accelerate [o]
  (let [f (:force o)
        m (:mass o)
        v (:velocity o)
        av (vector/add v (vector/scale f (/ 1.0 m)))]
    (assoc o :velocity av)))

(defn accelerate-all [world]
  (map accelerate world))

(defn reposition [o]
  (let [p (:position o)
        v (:velocity o)]
    (assoc o :position (position/add p v))))

(defn reposition-all [world]
  (map reposition world))

(defn close-enough? [[x1 y1] [x2 y2]]
  (> 30 (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))))

(defn collided? [o1 o2]
  (let [p1 (:position o1)
        p2 (:position o2)]
    (if (close-enough? p1 p2)
      (let [mass (+ (:mass o1) (:mass o2))
            distance (position/distance p1 p2)
            radius (Math/sqrt mass)]
        (>= (max 3 radius) distance))
      false)))

(defn center-of-mass [{p1 :position, m1 :mass}
                      {p2 :position, m2 :mass}]
  (let [s (/ m1 (+ m1 m2))
        uv (vector/unit (vector/subtract p2 p1))
        d (vector/scale uv s)]
    (position/add p1 d)))

(defn merge [{n1 :name, m1 :mass, v1 :velocity f1 :force, :as o1}
             {n2 :name, m2 :mass, v2 :velocity f2 :force, :as o2}]
  (let [p (center-of-mass o1 o2)
        m (+ m1 m2)
        mv1 (vector/scale v1 m1)
        mv2 (vector/scale v2 m2)
        v (vector/scale (vector/add mv1 mv2) (/ 1 m))
        f (vector/add f1 f2)
        n (if (> m1 m2) (str n1 "." n2) (str n2 "." n1))]
    (make p m v f n)))

(defn remove-obj [o world]
  (remove #(= o %) world))

(defn difference-list [l1 l2]
  (reduce #(remove-obj %2 %1) l1 l2))

(defn collide-all [world]
  (loop [colliding-world world collided-world [] collisions []]
    (if (empty? colliding-world)
      [collisions collided-world]
      (let [impactor (first colliding-world)
            targets (rest colliding-world)
            colliders (filter #(collided? impactor %) targets)
            merger (reduce merge impactor colliders)
            survivors (difference-list targets colliders)
            new-collisions (if (empty? colliders) collisions (conj collisions (:position merger)))]
        (recur survivors (conj collided-world merger) new-collisions)))))

(defn update-all [world]
  (let [[collisions collided-world] (collide-all world)]
    [collisions (-> collided-world calculate-forces-on-all accelerate-all reposition-all)]))
