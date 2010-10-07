(ns orbitmain
  (:use orbit.world)
  (:gen-class))

(defn -main [& args]
  (run-world))
