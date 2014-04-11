(ns physics (:use clojure.test))
(require 'physics.physics)

(deftest position-test
  (testing "creation"
    (is (position/origin? (position/make)))
    (is (= (position/make 1 1) (position/make 1 1)))
    (is (= 1 (first (position/make 1 0))))
    (is (= 0 (second (position/make 1 0))))
    )

  (testing "addition"
    (is (=
      (position/make 2 2)
      (position/add
        (position/make 1 1)
        (position/make 1 1))))
    )

  (testing "subtraction"
    (is (=
      (position/make 1 1)
      (position/subtract
        (position/make 3 4)
        (position/make 2 3))))
    )

  (testing "distance"
    (is (=
      1.0
      (position/distance
        (position/make 0 0)
        (position/make 0 1))))

    (is (=
      1.0
      (position/distance
        (position/make 1 0)
        (position/make 2 0))))

    (is (=
      5.0
      (position/distance
        (position/make 0 0)
        (position/make 3 4))))
  )
)
