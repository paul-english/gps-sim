(ns gps-sim.utils.numeric
  (:require [schema.macros :as sm])
  (:import [java.math RoundingMode]))

(sm/defn round-places :- Double
  [decimals :- Long
   number :- Number]
  (let [factor (Math/pow 10 decimals)]
    ;; TODO this seems like a shit way to deal with the rounding issues..
    ;; we should use bigdec everywhere really
    (/ (.setScale (bigdec (* factor number)) 0
                  RoundingMode/HALF_UP)
       factor)))

(comment
  ;; TODO
  (defn round [s n]
    (.setScale (bigdec n) s java.math.RoundingMode/HALF_EVEN)))

(sm/defn round :- Long
  [number :- Number]
  (->> number
       (round-places 0)
       long))

(sm/defn num-decimals :- Long
  [number :- Number]
  (-> (- number (Math/floor number))
      str
      count
      (- 2)))

(sm/defn approx= :- Boolean
  [a :- Number
   b :- Number]
  (let [error 0.000001]
    (and (<= a (+ b error))
         (>= a (- b error)))))

;; TODO schema?
(defn step-range
  "This version of range deals with annoying rounding errors
that clojure.core/range doesn't handle well, and is inclusive."
  [step]
  (let [reciprocal (round (/ 1 step))
        steps (map #(* step %)
                   (range 0 reciprocal))]
    (if (approx= (last steps) 1)
      steps
      (concat steps [1.0]))))
