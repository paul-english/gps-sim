(ns gps-sim.utils.angles
  (:refer-clojure :exclude [* - / + ==])
  (:use clojure.core.matrix
        clojure.core.matrix.operators)
  (:require [gps-sim.utils.matrix :refer [join-1 join-1-interleave]]))

;; TODO may not need
;;(defn ** [n k] (Math/pow n (or k 2)))

;; TODO not used yet
(defn x [rho phi theta] (* rho (Math/sin phi) (Math/cos theta)))
(defn y [rho phi theta] (* rho (Math/sin phi) (Math/sin theta)))
(defn z [rho phi theta] (* rho (Math/cos phi)))

;; TODO not used yet
(defn rho [x y z] (Math/sqrt (+ (** x) (** y) (** z))))
(defn phi [x y z] (Math/atan (/ y x)))
(defn theta [x y z] (Math/acos (/ z (rho x y z))))

;; TODO set by data instead
(def pi Math/PI)
(def tau (* 2 pi))

;; TODO schema input & output
(defn dms->radians ;;:- DMSMatrix
  [A               ;;:- RadianMatrix
   ]
  (let [
        times (mmul A (transpose [[1 0 0 0 0 0 0 0 0 0]]))
        orientations (mmul A (transpose [[0 0 0 0 1 0 0 0 0 0]
                                         [0 0 0 0 0 0 0 0 1 0]]))
        ;; TODO you can combine orientations & pi/180 into this transform
        degrees (mmul A (transpose [[0 1 (double 1/60) (double 1/3600) 0 0 0 0 0 0]
                                    [0 0 0 0 0 1 (double 1/60) (double 1/3600) 0 0]]))
        heights (mmul A (transpose [[0 0 0 0 0 0 0 0 0 1]]))
        radians (* degrees orientations (repeat (first (shape A))
                                                [(/ pi 180) (/ pi 180)]))


        ]
    ;;(println "--- dms->radians")
    ;;(println "orientatiosn" orientations)
    ;;(println "degrees" degrees)
    ;;(println "heights" heights)
    ;;(println "radians" radians)
    (join-1 times radians heights)
    )
  )

;; TODO input & output schemas
(defn radians->dms [A]
  (let [times (mmul A (transpose [[1 0 0 0]]))
        heights (mmul A (transpose [[0 0 0 1]]))
        ;; first
        degrees-decimal (mmul A (transpose [[0 (/ 180 pi) 0 0]
                                            [0 0 (/ 180 pi) 0]]))
        orientations (emap #(if (pos? %) 1 -1) degrees-decimal)
        positive-degrees (emap abs degrees-decimal)
        degrees (emap #(Math/floor %) positive-degrees)
        minutes-decimal (* 60 (- positive-degrees degrees))
        minutes (emap #(Math/floor %) minutes-decimal)
        seconds (* 60 (- minutes-decimal minutes))]
    ;;(println "--- radians->dms")
    ;;(println "shape a" (shape A))
    ;;(println "deg trans mat")
    ;;(pm (transpose (repeat (first (shape A)) [0 (/ 180 pi) (/ 180 pi) 0])))
    ;;(println "degrees-decimal" degrees-decimal)
    ;;(println "degrees" degrees)
    ;;(println "minutes-decimal" minutes-decimal)
    ;;(println "minutes" minutes)
    ;;(println "seconds" seconds)
    ;;(println "orientation" orientations)
    (join-1 times
            (join-1-interleave degrees minutes seconds orientations)
            heights)))
