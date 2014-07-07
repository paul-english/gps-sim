(ns gps-sim.receiver
  (:gen-class)
  (:refer-clojure :exclude [* - / + ==])
  (:use clojure.core.matrix
        clojure.core.matrix.operators)
  (:require [schema.macros :as sm]
            [schema.coerce :as coerce]
            ;; TODO
            ;;[clatrix.core :refer [norm]]
            [gps-sim.constants :refer [read-constants! tau s c]]
            [gps-sim.utils.coordinates :refer [dms->radians
                                               rad->cartesian
                                               cartesian->rad
                                               radians->dms]]
            [gps-sim.utils.io :refer [file->matrix stdin->matrix matrix->stdout]]
            [gps-sim.utils.matrix :refer [rotation-matrix
                                          norm
                                          column-map]]
            [gps-sim.utils.schemas :refer [DMSCoordinateList
                                           DataFile
                                           CartesianSatelliteList
                                           parse-data
                                           parse-dms-list
                                           parse-cartesian-satellite-list
                                           parse-cartesian-list]]
            [gps-sim.utils.numeric :refer [round-places] :as numeric]
            [gps-sim.utils.sequences :refer [partition-when]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def b12 (->> [[0M 40 45 55.0M 1 111 50 58.0M -1 1372.0M]]
              dms->radians
              rad->cartesian
              first))

(def max-iterations 30)

(defn round-seconds
  "Ensures that the seconds values in our output matrix are rounded."
  [A]
  (->> A
       (column-map numeric/round 3)
       (column-map numeric/round 7)))

(sm/defn group-by-index-change :- [CartesianSatelliteList]
  "Input for this program comes in as a list of multiple satellites for each
path point we need to interpolate. This function groups the input when our
satellite index changes."
  [satellites :- CartesianSatelliteList]
  (let [last-idx (dec (count satellites))
        compare-with-next (fn [idx item]
                            (cons (and (< idx last-idx)
                                       (> (first item)
                                          (first (nth satellites (inc idx)))))
                                  item))]
    (->> satellites
         (map-indexed compare-with-next)
         (partition-when first)
         (map #(map (fn [i] (drop 1 i)) %)))))

;; TODO this is just a dot product can probably use corematrix fn
(defn squared-error [m]
  (->> m
       (map #(** % 2))
       (apply +)))

(defn satellite-distances [vehicle satellites]
  (map #(distance % vehicle)
       satellites))

(defn A [N times satellite-range]
  (mapcat #(- (nth N (inc %))
              (nth N %)
              (* @c
                 (- (nth times %)
                    (nth times (inc %)))))
          satellite-range))

(defn gradient-A [satellites N vehicle satellite-range]
  (map #(let [current-sat (nth satellites %)
              next-sat (nth satellites (inc %))
              current-N (nth N %)
              next-N (nth N (inc %))]
          (- (/ (- current-sat vehicle)
                current-N)
             (/ (- next-sat vehicle)
                next-N)))
       satellite-range))

(defn trilaterate-vehicle [satellites]
  (let [start-time (nth (first satellites) 1)
        time-diff 0.05
        path-time (+ start-time time-diff)
        theta (/ (* @tau path-time) @s)
        rotation (rotation-matrix (bigdec theta))
        path-point (loop [vehicle-position (mmul rotation b12)
                          iterations 0]
                     (let [satellite-times (mmul satellites (transpose [[0 1 0 0 0]]))
                           satellites-cartesian (mmul satellites (transpose [[0 0 1 0 0]
                                                                             [0 0 0 1 0]
                                                                             [0 0 0 0 1]]))
                           N (satellite-distances vehicle-position satellites-cartesian)
                           sat-range (range 0 (dec (count satellites)))
                           A (A N satellite-times sat-range)
                           coordinates (gradient-A satellites-cartesian N vehicle-position sat-range)

                           grad (->> (map * A coordinates)
                                     (apply +))
                           dx-dx (apply + (map #(let [current-sat (nth satellites-cartesian %)
                                                      next-sat (nth satellites-cartesian (inc %))
                                                      current-coordinates (nth coordinates %)
                                                      current-N (nth N %)
                                                      next-N (nth N (inc %))]
                                                  (+ (** (nth current-coordinates 0) 2)
                                                     (* (nth A %)
                                                        (- (/ (- (** next-N 2)
                                                                 (** (- (nth next-sat 0) (nth vehicle-position 0)) 2))
                                                              (** next-N 3))
                                                           (/ (- (** current-N 2)
                                                                 (** (- (nth current-sat 0) (nth vehicle-position 0)) 2))
                                                              (** current-N 3))))))
                                               sat-range))
                           dy-dx (apply + (map #(let [current-sat (nth satellites-cartesian %)
                                                      next-sat (nth satellites-cartesian (inc %))
                                                      current-coordinates (nth coordinates %)
                                                      current-N (nth N %)
                                                      next-N (nth N (inc %))]
                                                  (- (* (nth current-coordinates 0)
                                                        (nth current-coordinates 1))
                                                     (* (nth A %)
                                                        (- (/ (* (- (nth next-sat 0) (nth vehicle-position 0))
                                                                 (- (nth next-sat 1) (nth vehicle-position 1)))
                                                              (** next-N 3))
                                                           (/ (* (- (nth current-sat 0) (nth vehicle-position 0))
                                                                 (- (nth current-sat 1) (nth vehicle-position 1)))
                                                              (** current-N 3))))))
                                               sat-range))
                           dz-dx (apply + (map #(let [current-sat (nth satellites-cartesian %)
                                                      next-sat (nth satellites-cartesian (inc %))
                                                      current-coordinates (nth coordinates %)
                                                      current-N (nth N %)
                                                      next-N (nth N (inc %))]
                                                  (- (* (nth current-coordinates 0)
                                                        (nth current-coordinates 2))
                                                     (* (nth A %)
                                                        (- (/ (* (- (nth next-sat 0) (nth vehicle-position 0))
                                                                 (- (nth next-sat 2) (nth vehicle-position 2)))
                                                              (** next-N 3))
                                                           (/ (* (- (nth current-sat 0) (nth vehicle-position 0))
                                                                 (- (nth current-sat 2) (nth vehicle-position 2)))
                                                              (** current-N 3))))))
                                               sat-range))
                           dx-dy dy-dx
                           dy-dy (apply + (map #(let [current-sat (nth satellites-cartesian %)
                                                      next-sat (nth satellites-cartesian (inc %))
                                                      current-coordinates (nth coordinates %)
                                                      current-N (nth N %)
                                                      next-N (nth N (inc %))]
                                                  (+ (** (nth current-coordinates 1) 2)
                                                     (* (nth A %)
                                                        (- (/ (- (** next-N 2)
                                                                 (** (- (nth next-sat 1) (nth vehicle-position 1)) 2))
                                                              (** next-N 3))
                                                           (/ (- (** current-N 2)
                                                                 (** (- (nth current-sat 1) (nth vehicle-position 1)) 2))
                                                              (** current-N 3))))))
                                               sat-range))
                           dz-dy (apply + (map #(let [current-sat (nth satellites-cartesian %)
                                                      next-sat (nth satellites-cartesian (inc %))
                                                      current-coordinates (nth coordinates %)
                                                      current-N (nth N %)
                                                      next-N (nth N (inc %))]
                                                  (- (* (nth current-coordinates 1)
                                                        (nth current-coordinates 2))
                                                     (* (nth A %)
                                                        (- (/ (* (- (nth next-sat 1) (nth vehicle-position 1))
                                                                 (- (nth next-sat 2) (nth vehicle-position 2)))
                                                              (** next-N 3))
                                                           (/ (* (- (nth current-sat 1) (nth vehicle-position 1))
                                                                 (- (nth current-sat 2) (nth vehicle-position 2)))
                                                              (** current-N 3))))))
                                               sat-range))
                           dx-dz dz-dx
                           dy-dz dz-dy
                           dz-dz (apply + (map #(let [current-sat (nth satellites-cartesian %)
                                                      next-sat (nth satellites-cartesian (inc %))
                                                      current-coordinates (nth coordinates %)
                                                      current-N (nth N %)
                                                      next-N (nth N (inc %))]
                                                  (+ (** (nth current-coordinates 2) 2)
                                                     (* (nth A %)
                                                        (- (/ (- (** next-N 2)
                                                                 (** (- (nth next-sat 2) (nth vehicle-position 2))
                                                                     2))
                                                              (** next-N 3))
                                                           (/ (- (** current-N 2)
                                                                 (** (- (nth current-sat 2) (nth vehicle-position 2))
                                                                     2))
                                                              (** current-N 3))))))
                                               sat-range))
                           second-partials [[dx-dx dy-dx dz-dx]
                                            [dx-dy dy-dy dz-dy]
                                            [dx-dz dy-dz dz-dz]]
                           J-0-0 (sqrt (mget second-partials 0 0))
                           J-1-0 (/ (mget second-partials 1 0)
                                    J-0-0)
                           J-2-0 (/ (mget second-partials 2 0)
                                    J-0-0)
                           J-1-1 (sqrt (- (mget second-partials 1 1)
                                          (** J-1-0 2)))
                           J-2-1 (/ (- (mget second-partials 2 1)
                                       (* J-2-0 J-1-0))
                                    J-1-1)
                           J-2-2 (sqrt (- (mget second-partials 2 2)
                                          (** J-2-0 2)
                                          (** J-2-1 2)))
                           J [[J-0-0 0 0]
                              [J-1-0 J-1-1 0]
                              [J-2-0 J-2-1 J-2-2]]
                           ng-0 (/ (mget grad 0)
                                   (mget J 0 0))
                           ng-1 (/ (- (mget grad 1)
                                      (* (mget J 1 0) ng-0))
                                   (mget J 1 1))
                           ng-2 (/ (- (mget grad 2)
                                      (* (mget J 2 0) ng-0)
                                      (* (mget J 2 1) ng-1))
                                   (mget J 2 2))
                           ng-2 (/ ng-2 (mget J 2 2))
                           ng-1 (/ (- ng-1
                                      (* (mget J 2 1) ng-2))
                                   (mget J 1 1))
                           ng-0 (/ (- ng-0
                                      (* (mget J 2 0) ng-2)
                                      (* (mget J 1 0) ng-1))
                                   (mget J 0 0))
                           next-guess [ng-0 ng-1 ng-2]
                           new-vehicle-position (- vehicle-position
                                                   next-guess)
                           next-norm (norm next-guess)]
                       (if (or (and (< next-norm 0.1)
                                    (< (squared-error A) 0.1))
                               (> iterations max-iterations))
                         new-vehicle-position
                         (recur new-vehicle-position
                                (inc iterations)))))

        theta (/ (* (- @tau) path-time) @s)
        rotation (rotation-matrix (bigdec theta))
        ;; TODO check if path-point is above the horizon
        ;; TODO better names
        path-point (mmul rotation path-point)
        path-rad (cartesian->rad [(bigdec path-time)]
                                 (parse-cartesian-list [path-point]))
        path-dms (radians->dms path-rad)
        path-rounded (round-seconds path-dms)]
    path-rounded))

(sm/defn run :- DMSCoordinateList
  [data :- DataFile
   input :- CartesianSatelliteList]
  (read-constants! data)
  (->> input
       group-by-index-change
       (mapcat trilaterate-vehicle)))

(defn -main [& args]
  (let [data (-> "data.dat"
                 file->matrix
                 (get-column 0)
                 parse-data)]
    (->> (stdin->matrix)
         parse-cartesian-satellite-list
         (run data)
         matrix->stdout)
    :ok))
