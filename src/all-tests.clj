(ns all-tests
  (:use clojure.test)
  (:require physics.vector-test physics.position-test physics.object-test orbit.world-test))
(run-tests 'vector-test 'position-test 'object-test 'world-test)
