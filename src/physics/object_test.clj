(ns physics (:use clojure.test))
(require ['physics.position :as 'position])
(use 'physics.test-utilities)

(deftest object-test
  (let [
    v0 (vector/make)
    v11 (vector/make 1 1)
    o1 (object/make (position/make 1 1) 2 v0 v0 "o1")
    o2 (object/make (position/make 1 2) 3 v0 v0 "o2")
    o3 (object/make (position/make 4 5) 4 v0 v0 "o3")
    os [o1 o2 o3]
    ]
    (testing "default creation"
      (let [o (object/make)]
        (is (position/origin? (:position o)))
        (is (= 0 (:mass o)))
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
        (is (= mass (:mass o)))
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
        acumulated-o1 (object/accumulate-forces o1 os)
        expected (vector/add (object/force-between o1 o2) (object/force-between o1 o3))
        ]
        (is (vector-close-to
          expected
          (:force acumulated-o1)))
        )
      )

    (testing "calculate-forces-on-all"
      (let [
        fs (object/calculate-forces-on-all os)
        ]
        (is (= 3 (count fs)))
        (is (= (nth fs 0) (object/accumulate-forces o1 os)))
        (is (= (nth fs 1) (object/accumulate-forces o2 os)))
        (is (= (nth fs 2) (object/accumulate-forces o3 os)))
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
        as (object/accelerate-all (object/calculate-forces-on-all os))
        ]
        (is (= 3 (count os)))
        (is (= (nth as 0) (-> o1 (object/accumulate-forces os) object/accelerate)))
        (is (= (nth as 1) (-> o2 (object/accumulate-forces os) object/accelerate)))
        (is (= (nth as 2) (-> o3 (object/accumulate-forces os) object/accelerate)))
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
        rs (-> os object/calculate-forces-on-all object/accelerate-all object/reposition-all)
        ]
        (is (= 3 (count os)))
        (is (= (nth rs 0) (-> o1 (object/accumulate-forces os) object/accelerate object/reposition)))
        (is (= (nth rs 1) (-> o2 (object/accumulate-forces os) object/accelerate object/reposition)))
        (is (= (nth rs 2) (-> o3 (object/accumulate-forces os) object/accelerate object/reposition)))
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
        (is (= (position/make 1 1.4) (:position om)))
        (is (= 5 (:mass om)))
        (is (= (vector/make -1/5 0) (:velocity om)))
        (is (= (vector/make 2 2) (:force om)))
        )
      )

    (testing "collide"
      (let [
        cos (object/collide o1 o2 os)
        ]
        (is (= 2 (count cos)))
        (is (some #(= (object/merge o1 o2) %) cos))
        (is (some #(= o3 %) cos))
        )
      )

    (testing "collide-all"
      (let [
        cos (object/collide-all os)
        ]
        (is (= 2 (count cos)))
        (is (some #(= (object/merge o1 o2) %) cos))
        (is (some #(= o3 %) cos))        
        )
      )
    )
  )



