(ns gps-sim.utils.coordinates
  (:refer-clojure :exclude [* - / + ==])
  (:use clojure.core.matrix
        clojure.core.matrix.operators)
  (:require [schema.macros :as sm]
            [gps-sim.constants :refer [pi R tau]]
            [gps-sim.utils.schemas :refer [CartesianCoordinateList
                                           DMSCoordinateList
                                           RadCoordinateList
                                           parse-rad-list
                                           parse-dms-list
                                           parse-cartesian-list]]
            [gps-sim.utils.matrix :refer [join-1 join-1-interleave]]
            [gps-sim.utils.numeric :refer [round-places]]))

(sm/defn dms->radians :- RadCoordinateList
  [A :- DMSCoordinateList]
  (with-precision 20
    (let [->rad (/ @pi 180)
          deg->rad (* 1 ->rad)
          m->rad (* 1/60 ->rad)
          s->rad (* 1/3600 ->rad)
          times (mmul A (transpose [[1 0 0 0 0 0 0 0 0 0]]))
          orientations (mmul A (transpose [[0 0 0 0 1 0 0 0 0 0]
                                           [0 0 0 0 0 0 0 0 1 0]]))
          radians (->> [[0 deg->rad m->rad s->rad 0 0 0 0 0 0]
                        [0 0 0 0 0 deg->rad m->rad s->rad 0 0]]
                       transpose
                       (mmul A)
                       (* orientations))
          heights (mmul A (transpose [[0 0 0 0 0 0 0 0 0 1]]))]
      (parse-rad-list
       (join-1 times radians heights)))))

(sm/defn radians->dms :- DMSCoordinateList
  [A :- RadCoordinateList]
  (let [times (mmul A (transpose [[1 0 0 0]]))
        heights (mmul A (transpose [[0 0 0 1]]))
        radians (emap #(if (or (< % (- @pi)) (> % @pi))
                         (with-precision 20
                             (let [i (int (/ (+ % @pi) @tau))]
                               (- % (* (if (< % 0) (dec i) i)
                                       @tau))))
                         %)
                      A)
        degrees-decimal (->> [[0 (/ 180 @pi) 0 0]
                              [0 0 (/ 180 @pi) 0]]
                             (with-precision 20)
                             transpose
                             (mmul radians))
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

(sm/defn rad->cartesian :- CartesianCoordinateList
  [A :- RadCoordinateList]
  (let [psi (get-column A 1)
        lambda (get-column A 2)
        h (get-column A 3)
        rho (+ @R h)
        x (* rho (cos psi) (cos lambda))
        y (* rho (cos psi) (sin lambda))
        z (* rho (sin psi))]
    (parse-cartesian-list (transpose [x y z]))))

(sm/defn cartesian->rad :- RadCoordinateList
  [times :- [BigDecimal]
   A :- CartesianCoordinateList]
  (let [x (get-column A 0)
        y (get-column A 1)
        z (get-column A 2)
        rho (sqrt (+ (** x 2) (** y 2) (** z 2)))
        psi (map (fn [z rho]
                 (cond
                  (not (= rho 0)) (with-precision 20 (asin (/ z rho)))
                  (> z 0) (/ @pi 2)
                  (< z 0) (- (/ @pi 2))))
               z rho)
        lambda (map (fn [x y]
                 (cond
                  (and (= x 0) (> y 0)) (/ @pi 2)
                  (and (= x 0) (< y 0)) (- (/ @pi 2))
                  (and (> x 0) (> y 0)) (with-precision 20
                                          (atan (/ y x)))
                  (and (> x 0) (< y 0)) (with-precision 20
                                          (+ @tau (atan (/ y x))))
                  (< x 0) (with-precision 20
                            (+ @pi (atan (/ y x))))))
               x y)
        h (- rho @R)]
    (parse-rad-list (transpose [times psi lambda h]))))
