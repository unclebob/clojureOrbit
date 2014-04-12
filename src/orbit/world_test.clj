(ns world-test
  (:use clojure.test orbit.world))

(deftest world-test
  (testing "collision aging"
    (is (= (age-collisions [[5 :x]]) [[4 :x]]))
    (is (= (age-collisions [[1 :x] [2 :y]]) [[1 :y]]))))
