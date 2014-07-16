(ns gps-sim.utils.numeric
  (:import [java.math RoundingMode]))

(defn round-places
  [decimals number]
  (let [factor (Math/pow 10 decimals)]
    (/ (.setScale (bigdec (* factor number)) 0
                  RoundingMode/HALF_UP)
       factor)))

(defn round [number]
  (->> number
       (round-places 0)))

(defn num-decimals
  [number]
  (-> (- number (Math/floor number))
      str
      count
      (- 2)))

(defn approx= [a b & {:keys [error]
                      :or {error 0.000001}}]
  (<= (- b error) a (+ b error)))

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
