(ns gps-sim.utils.matrix
  (:refer-clojure :exclude [* - / + ==])
  (:use clojure.core.matrix
        clojure.core.matrix.operators)
  (:require [schema.macros :as sm]
            [gps-sim.constants :refer [tau]]
            [gps-sim.utils.numeric :refer [round-places num-decimals] :as num]))

(defn lerp
  "Performs a linear interpolation of all the values in A.
Expects A to be an 2xm sized matrix, where the first row
is the start vector of interpolation and the second row
is the end vector. Note, the name lerp is used in other
contexts and it may not mean the same kind of linear
interpolation that is being performed here.

(lerp [[1] [5]] 1) => [[1] [2] [3] [4] [5]]"
  [A step]
  {:pre [(= 2 (row-count A))
         (<= step 1)
         (> step 0)]}
  (let [precision (num-decimals step)
        reciprocal (/ 1 step)
        interpolation (num/step-range step)
        transformation (transpose (matrix [(reverse interpolation)
                                           interpolation]))]
    (emap (partial round-places precision)
          (mmul transformation A))))

(defn join-1
  "core.matrix hasn't implemented join along dimension
1 yet, so we do this inefficiently."
  [& matrices]
  (apply mapv concat matrices))

(defn join-1-interleave
  "There are probably better ways handle matrices, so
that you don't need an interleaving join."
  [& matrices]
  (apply mapv interleave matrices))

(sm/defn rotation-matrix [theta :- Double]
  [[(cos theta) (- (sin theta)) 0]
   [(sin theta) (cos theta) 0]
   [0 0 1]])

(defn norm
  "A naive implementation of the vector 2-norm. An interface
for the norm is currently in 0.24.1-SNAPSHOT under
clojure.core.matrix.linear and it's probably best to use that
once it's released."
  ([v] (->> v
            (map #(** % 2))
            (apply +)
            sqrt))
  ([a b] (->> (map #(* %1 %2) a b)
              (apply +)
              sqrt)))

(defn squared-norm [m]
  (->> m
       (map #(** % 2))
       (apply +)))

(defn column-map
  "Map a function a column of a matrix"
  [f i m]
  (->> (get-column m i)
       (emap f)
       (set-column m i)))
