(ns gps-sim.vehicle-test
  (:use midje.sweet
        gps-sim.vehicle)
  (:require [clojure.core.matrix :refer [matrix? shape emap pm]]
            [clojure.java.shell :refer [sh]]
            [gps-sim.utils.numeric :refer [approx=]]
            [gps-sim.test-data :refer [vehicle-expected-output]]))

(with-state-changes [(around :facts (schema.macros/with-fn-validation ?form))]
  (facts "Vehicle"
    (fact "Program digests stdin"
      (with-in-str "102123.0 112000.0 90 40 48 44 1 111 48 40 -1 2377\n"
        (-main) => :ok))

    (fact "Program creates a linear interpolation from a hardcoded start point to our input point"
      (let [output (run [[102123.0M 112000.0M 90 40 48 44 1 111 48 40 -1 2377]])]
        (matrix? output) => true
        (shape output) => [91 10]
        output => vehicle-expected-output))

    (fact "Program produces the same output as the supplied program"
      (let [data-files (->> ["b12" "b12t0" "bm" "np" "o" "sp"]
                            (map #(slurp (str "data/" % ".dat"))))]
        (doseq [input data-files]
          (sh "java" "vehicle"
              :in input
              :dir "data") => ""
          )

        )


      )
    ))
