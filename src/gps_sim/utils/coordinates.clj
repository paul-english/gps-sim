(ns gps-sim.utils.coordinates
  (:refer-clojure :exclude [* - / + ==])
  (:use clojure.core.matrix
        clojure.core.matrix.operators)
  (:require [schema.core :as s]
            [gps-sim.constants :refer [pi R]]
            [gps-sim.utils.schemas :refer [CartesianCoordinateList
                                           DMSCoordinateList
                                           RadCoordinateList
                                           parse-rad-list
                                           parse-dms-list
                                           parse-cartesian-list]]
            [gps-sim.utils.matrix :refer [join-1 join-1-interleave]]))

(s/defn dms->radians :- RadCoordinateList
  [A :- DMSCoordinateList]
  (let [times (mmul A (transpose [[1 0 0 0 0 0 0 0 0 0]]))
        orientations (mmul A (transpose [[0 0 0 0 1 0 0 0 0 0]
                                         [0 0 0 0 0 0 0 0 1 0]]))
        degrees (with-precision 20
                  (mmul A (transpose [[0 1 1/60 1/3600 0 0 0 0 0 0]
                                      [0 0 0 0 0 1 1/60 1/3600 0 0]])))
        heights (mmul A (transpose [[0 0 0 0 0 0 0 0 0 1]]))

        radians (* degrees orientations (repeat (first (shape A))
                                                (with-precision 20
                                                  [(/ @pi 180) (/ @pi 180)])))]
    (parse-rad-list
     (join-1 times radians heights))))

(s/defn radians->dms :- DMSCoordinateList
  [A :- RadCoordinateList]
  (let [times (mmul A (transpose [[1 0 0 0]]))
        heights (mmul A (transpose [[0 0 0 1]]))
        degrees-decimal (->> [[0 (/ 180 @pi) 0 0]
                              [0 0 (/ 180 @pi) 0]]
                             (with-precision 20)
                             transpose
                             (mmul A))
        orientations (emap #(if (pos? %) 1 -1) degrees-decimal)
        positive-degrees (emap abs degrees-decimal)
        degrees (emap #(Math/floor %) positive-degrees)
        minutes-decimal (* 60 (- positive-degrees degrees))
        minutes (emap #(Math/floor %) minutes-decimal)
        seconds (* 60 (- minutes-decimal minutes))]
    (parse-dms-list
     (join-1 times
             (join-1-interleave degrees minutes seconds orientations)
             heights))))

(s/defn rad->cartesian :- CartesianCoordinateList
  [A :- RadCoordinateList]
  (let [psi (get-column A 1)
        lambda (get-column A 2)
        h (get-column A 3)
        rho (+ @R h)
        x (* rho (cos psi) (cos lambda))
        y (* rho (cos psi) (sin lambda))
        z (* rho (sin psi))]
    (parse-cartesian-list (transpose [x y z]))))

(s/defn cartesian->rad :- RadCoordinateList
  [times :- [BigDecimal]
   A :- CartesianCoordinateList]
  (let [x (get-column A 0)
        y (get-column A 1)
        z (get-column A 2)
        rho (sqrt (+ (** x 2) (** y 2) (** z 2)))
        psi (with-precision 20 (asin (/ z rho)))
        lambda (with-precision 20 (atan (/ y x)))
        h (- rho @R)]
    (parse-rad-list (transpose [times psi lambda h]))))
