(ns object-test
  (:use clojure.test physics.test-utilities)
  (:require [physics.vector :as vector]
            [physics.object :as object]
            [physics.position :as position])
  (:import physics.object.object))

(defn world-momentum [world]
  (reduce vector/add (map #(vector/scale (:velocity %) (:mass %)) world)))

(defn world-energy [world]
  (reduce + (map #(* 0.5 (:mass %) (square (vector/magnitude (:velocity %)))) world)))

(deftest object-test
  (let [
         v0 (vector/make)
         v11 (vector/make 1 1)
         o1 (object/make (position/make 1 1) 2 v0 v0 "o1")
         o2 (object/make (position/make 1 2) 3 v0 v0 "o2")
         o3 (object/make (position/make 10 10) 4 v0 v0 "o3")
         world [o1 o2 o3]
         ]
    (testing "default creation"
      (let [o (object/make)]
        (is (position/origin? (:position o)))
        (is (== 0 (:mass o)))
        (is (= v0 (:velocity o)))
        (is (= v0 (:force o)))
        (is (= "TILT" (:name o)))
        )
      )

    (testing "defined creation"
      (let [pos (position/make 1 1)
            mass 2
            velocity (vector/make 2 2)
            force (vector/make 3 3)
            name "name"
            o (object/make pos mass velocity force name)]
        (is (= pos (:position o)))
        (is (== mass (:mass o)))
        (is (= velocity (:velocity o)))
        (is (= force (:force o)))
        (is (= name (:name o)))
        )
      )

    (testing "gravity"
      (is (=
            (/ 6 16)
            (object/gravity 2 3 4))))

    (testing "force-between"
      (let [
             c3r2 (/ 3 (Math/sqrt 2))
             o1 (object/make (position/make 1 1) 2 v0 v0 "o1")
             o2 (object/make (position/make 2 2) 3 v0 v0 "o2")]
        (is (vector-close-to
              (vector/make c3r2 c3r2)
              (object/force-between o1 o2)))
        )
      )

    (testing "accumulate-forces"
      (let [
             acumulated-o1 (object/accumulate-forces o1 world)
             expected (vector/add (object/force-between o1 o2) (object/force-between o1 o3))
             ]
        (is (vector-close-to
              expected
              (:force acumulated-o1)))
        )
      )

    (testing "calculate-forces-on-all"
      (let [
             fs (object/calculate-forces-on-all world)
             ]
        (is (= 3 (count fs)))
        (is (= (nth fs 0) (object/accumulate-forces o1 world)))
        (is (= (nth fs 1) (object/accumulate-forces o2 world)))
        (is (= (nth fs 2) (object/accumulate-forces o3 world)))
        )
      )

    (testing "accelerate"
      (let [
             o (object/make (position/make) 2 v11 v11 "o1")
             ao (object/accelerate o)
             ]
        (is (vector-close-to (vector/make 1.5 1.5) (:velocity ao)))
        )
      )

    (testing "accelerate-all"
      (let [
             as (object/accelerate-all (object/calculate-forces-on-all world))
             ]
        (is (= 3 (count world)))
        (is (= (nth as 0) (-> o1 (object/accumulate-forces world) object/accelerate)))
        (is (= (nth as 1) (-> o2 (object/accumulate-forces world) object/accelerate)))
        (is (= (nth as 2) (-> o3 (object/accumulate-forces world) object/accelerate)))
        )
      )

    (testing "reposition"
      (let [
             o (object/make (position/make 1 1) 2 v11 v0 "o1")
             ro (object/reposition o)
             ]
        (is (= (position/make 2 2) (:position ro)))
        )
      )

    (testing "reposition-all"
      (let [
             rs (-> world object/calculate-forces-on-all object/accelerate-all object/reposition-all)
             ]
        (is (= 3 (count world)))
        (is (= (nth rs 0) (-> o1 (object/accumulate-forces world) object/accelerate object/reposition)))
        (is (= (nth rs 1) (-> o2 (object/accumulate-forces world) object/accelerate object/reposition)))
        (is (= (nth rs 2) (-> o3 (object/accumulate-forces world) object/accelerate object/reposition)))
        )
      )

    (testing "collided?"
      (is (object/collided? o1 o2))
      (is (not (object/collided? o1 o3)))
      )

    (testing "merge"
      (let [
             o1 (object/make (position/make 1 1) 2 (vector/make 1 0) (vector/make 1 1) "o1")
             o2 (object/make (position/make 1 2) 3 (vector/make -1 0) (vector/make 1 1) "o2")
             om (object/merge o1 o2)
             ]
        (is (= "o2.o1" (:name om)))
        (is (vector/equal (position/make 1 1.4) (:position om)))
        (is (== 5 (:mass om)))
        (is (vector/equal (vector/make -1/5 0) (:velocity om)))
        (is (vector/equal (vector/make 2 2) (:force om)))))

    (testing "difference-list"
      (is (= [1 3] (object/difference-list [1 2 3 4] [2 4]))))

    (testing "collide-all"
      (let [[collisions collided-world] (object/collide-all world)]
        (is (= 2 (count collided-world)))
        (is (some #(= (object/merge o1 o2) %) collided-world))
        (is (some #(= o3 %) collided-world))
        (is (= 1 (count collisions)))
        (is (= (:position (object/merge o1 o2)) (first collisions)))))

    (testing "update-all"
      (let [[collisions new-world] (object/update-all world)]
        (is (= 1 (count collisions)))
        (is (= 2 (count new-world)))
        (is (= #{"o2.o1" "o3"} (set (map :name new-world))))
        (is (vector-close-to (world-momentum world) (world-momentum new-world)))))
    )
  )



