(ns gps-sim.utils.matrix
  (:refer-clojure :exclude [* - / + ==])
  (:use clojure.core.matrix
        clojure.core.matrix.operators)
  (:require [schema.core :as s]
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
         (< step 1)
         (> step 0)]}
  (let [precision (num-decimals step)
        reciprocal (/ 1 step)
        interpolation (num/step-range step)
        transformation (transpose (matrix [(reverse interpolation)
                                           interpolation]))]
    ;;(println "-- lerp --")
    ;;(println precision)
    ;;(println "-" (count interpolation))
    ;;(println "- interpolation" (count interpolation) interpolation)
    ;;(pm (transpose [interpolation]))
    ;;(pm transformation)
    ;;(println "--" (first (mmul transformation A)))
    ;;(println "--" (last (mmul transformation A)))
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

(s/defn rotation-matrix [rotation :- BigDecimal]
  (let [theta (* @tau rotation)]
    ;;(println "--- rot-mat" theta)
    [[(cos theta)  (- (sin theta)) 0]
     [(sin theta)     (cos theta)  0]
     [      0        0     1]]))

;; TODO may not need
(defn spherical->cartesian-jacobian [rho theta phi]
  [[(* (sin theta) (cos phi))    (* rho (cos theta) (cos phi)) (- (* rho (sin theta) (sin phi)))]
   [(* (sin theta) (sin phi))    (* rho (cos theta) (sin phi))    (* rho (sin theta) (cos rho))]
   [   (cos theta)          (- (* rho (sin theta)))                               0]])

;; TODO may not need
(defn cartesian->spherical-jacobian [x y z]
  (let [rho (sqrt (+ (** x 2) (** y 2) (** z 2)))]
    [[(/ x rho) (/ y rho) (/ z rho)]
     [(/ (* x z) (* (** rho 2) (sqrt (+ (** x 2) (** y 2)))))
      (/ (* y z) (* (** rho 2) (sqrt (+ (** x 2) (** y 2)))))
      (- (/ (sqrt (+ (** x 2) (** y 2))) (** rho 2)))]
     [(/ (- y) (+ (** x 2) (** y 2)))
      (/ x (+ (** x 2) (** y 2)))
      0]]))
