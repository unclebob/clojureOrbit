(ns physics.test-utilities)

(defn square [n] (* n n))
(defn close-to [a b] (< (square(- a b)) 0.0001))
(defn vector-close-to [a b]
  (and (close-to (:x a) (:x b)) (close-to (:y a) (:y b))))
