(ns gps-sim.utils.sequences-test
  (:use midje.sweet
        gps-sim.utils.sequences))

(with-state-changes [(around :facts (schema.macros/with-fn-validation ?form))]
  (facts "Sequence operations"
    (fact "zipjuxt"
      ((zipjuxt inc dec) [1 2]) => [2 1])))
