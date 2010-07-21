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

    (testing "reposition"
      (let [
        o (object/make (position/make 1 1) 2 v11 v0 "o1")
        ro (object/reposition o)
        ]
        (is (= (position/make 2 2) (:position ro)))
        )
      )
    )
  )



