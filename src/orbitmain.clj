(ns orbitmain
  (:gen-class))

(use 'orbit.world)

(defn -main [& args]
  (run-world))
