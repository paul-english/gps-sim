(ns gps-sim.utils.numeric-test
  (:use midje.sweet
        gps-sim.utils.numeric))

(facts "Numeric operations"
  (fact "Rounding decimal numbers to a certain amount of places"
    (round-places 0 0.6) => 1.0
    (round-places 0 0.294857) => 0.0
    (round-places 2 0.294857) => 0.29
    (round-places 3 0.294857) => 0.295
    (round-places 4 0.294857) => 0.2949)
  (fact "Plain old rounding"
    (round 0.6) => 1.0
    (round 0.3) => 0.0)
  (fact "Getting the number of digits past a decimal point"
    (num-decimals 0.294857) => 6
    (num-decimals 0.1) => 1
    (num-decimals 0.0) => 1
    (num-decimals 0.00) => 1 ;; erm... ok
    (num-decimals 0.320) => 2)
  (fact "step-range"
    (step-range 0.5) => [0.0 0.5 1.0]
    (map (partial round-places 1)
         (step-range 0.2)) => [0.0 0.2 0.4 0.6 0.8 1.0])
  (fact "approx="
    (approx= 0.999999999 1) => true))
