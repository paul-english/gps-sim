(ns gps-sim.utils.coordinates
  (:refer-clojure :exclude [* - / + ==])
  (:use clojure.core.matrix
        clojure.core.matrix.operators)
  (:require [gps-sim.constants :refer [pi R tau]]
            [gps-sim.utils.matrix :refer [join-1 join-1-interleave]]
            [gps-sim.utils.numeric :refer [round-places]]))

(defn dms->radians [A]
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
    (join-1 times radians heights)))

(defn radians->dms [A]
  (let [times (mmul A (transpose [[1 0 0 0]]))
        heights (mmul A (transpose [[0 0 0 1]]))
        radians (emap #(if (or (< % (- @pi)) (> % @pi))
                         (let [i (int (/ (+ % @pi) @tau))]
                           (- % (* (if (< % 0) (dec i) i)
                                   @tau)))
                         %)
                      A)
        degrees-decimal (->> [[0 (/ 180 @pi) 0 0]
                              [0 0 (/ 180 @pi) 0]]
                             transpose
                             (mmul radians))
        orientations (emap #(if (pos? %) 1 -1) degrees-decimal)
        positive-degrees (emap abs degrees-decimal)
        degrees (emap #(int (Math/floor %)) positive-degrees)
        minutes-decimal (* 60 (- positive-degrees degrees))
        minutes (emap #(int (Math/floor %)) minutes-decimal)
        seconds (* 60 (- minutes-decimal minutes))]
    (join-1 times
            (join-1-interleave degrees minutes seconds orientations)
            heights)))

(defn rad->cartesian [A]
  (let [psi (get-column A 1)
        lambda (get-column A 2)
        h (get-column A 3)
        rho (+ @R h)
        _ (println "rho" rho)
        x (* rho (cos psi) (cos lambda))
        _ (println "x" x)
        y (* rho (cos psi) (sin lambda))
        z (* rho (sin psi))]
    (transpose [x y z])))

(defn cartesian->rad [times A]
  (let [x (get-column A 0)
        y (get-column A 1)
        z (get-column A 2)
        rho (sqrt (+ (** x 2) (** y 2) (** z 2)))
        psi (map (fn [z rho]
                 (cond
                  (not (= rho 0)) (asin (/ z rho))
                  (> z 0) (/ @pi 2)
                  (< z 0) (- (/ @pi 2))))
               z rho)
        lambda (map (fn [x y]
                 (cond
                  (and (= x 0) (> y 0)) (/ @pi 2)
                  (and (= x 0) (< y 0)) (- (/ @pi 2))
                  (and (> x 0) (> y 0)) (atan (/ y x))
                  (and (> x 0) (< y 0)) (+ @tau (atan (/ y x)))
                  (< x 0) (+ @pi (atan (/ y x)))))
               x y)
        h (- rho @R)]
    (transpose [times psi lambda h])))
