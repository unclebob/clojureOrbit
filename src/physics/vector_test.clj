(ns physics (:use clojure.test))
(require 'physics.physics)
(use 'physics.test-utilities)


(deftest vector-test
  (testing "vector creation"
    (is (= 0 (:x (vector/make))))
    (is (= 0 (:y (vector/make))))
    (is (= (vector/make 1 1) (vector/make 1 1)))
    (is (= 1 (:x (vector/make 1 0))))
    (is (= 0 (:y (vector/make 1 0))))
    )

  (testing "vector addition"
    (is (=
      (vector/make 2 2)
      (vector/add
        (vector/make 1 1)
        (vector/make 1 1))))
    )

  (testing "vector subtraction"
    (is (=
      (vector/make 1 2)
      (vector/subtract
        (vector/make 3 4)
        (vector/make 2 2))))
    )

  (testing "vector scaling"
    (is (=
      (vector/make 3 3)
      (vector/scale
        (vector/make 1 1)
        3)))
    )

  (testing "magnitude"
    (is (= 3 (vector/magnitude (vector/make 0 3))))
    (is (= 3 (vector/magnitude (vector/make 3 0))))
    (is (= 5 (vector/magnitude (vector/make 3 4))))
    )

  (testing "unit vector"
    (is (=
      (vector/make 0 1)
      (vector/unit (vector/make 0 99))))

    (is (=
      (vector/make 1 0)
      (vector/unit (vector/make 99 0))))

    (let [r2 (Math/sqrt (/ 1 2))]
      (is (vector-close-to
        (vector/make r2 r2)
        (vector/unit (vector/make 1 1))))) 
    )

  (testing "rotate90"
    (is (= (vector/make -1 2) (vector/rotate90 (vector/make 2 1)))))
  )


