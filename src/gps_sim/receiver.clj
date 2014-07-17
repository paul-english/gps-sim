(ns gps-sim.receiver
  (:gen-class)
  (:refer-clojure :exclude [* - / + ==])
  (:use clojure.core.matrix
        clojure.core.matrix.operators)
  (:require [clojure.java.io :as io]
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
                                          squared-norm
                                          column-map]]
            [gps-sim.utils.numeric :refer [round-places] :as numeric]
            [gps-sim.utils.sequences :refer [partition-when]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def b12 (->> [[0.0 40 45 55.0 1 111 50 58.0 -1 1372.0]]
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

(defn group-by-index-change
  "Input for this program comes in as a list of multiple satellites for each
path point we need to interpolate. This function groups the input when our
satellite index changes."
  [satellites]
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

(defn partial-derivatives [satellites-cartesian coordinates N vehicle-position sat-range A]
  (let [dx-dx (apply + (map #(let [current-sat (nth satellites-cartesian %)
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
                            sat-range))]
    [[dx-dx dy-dx dz-dx]
     [dx-dy dy-dy dz-dy]
     [dx-dz dy-dz dz-dz]]))

(defn jacobian [partials]
  (let [J-0-0 (sqrt (mget partials 0 0))
        J-1-0 (/ (mget partials 1 0)
                 (sqrt (mget partials 0 0)))
        J-2-0 (/ (mget partials 2 0)
                 (sqrt (mget partials 0 0)))
        J-1-1 (sqrt (- (mget partials 1 1)
                           (** (/ (mget partials 1 0)
                                  (sqrt (mget partials 0 0)))
                               2)))
        J-2-1 (/ (- (mget partials 2 1)
                    (* (/ (mget partials 2 0)
                          (sqrt (mget partials 0 0)))
                       (/ (mget partials 1 0)
                          (sqrt (mget partials 0 0)))))
                 (sqrt (- (mget partials 1 1)
                              (** (/ (mget partials 1 0)
                                     (sqrt (mget partials 0 0)))
                                  2))))
        J-2-2 (sqrt (- (mget partials 2 2)
                           (** (/ (mget partials 2 0)
                                  (sqrt (mget partials 0 0)))
                               2)
                           (** (/ (- (mget partials 2 1)
                                     (* (/ (mget partials 2 0)
                                           (sqrt (mget partials 0 0)))
                                        (/ (mget partials 1 0)
                                           (sqrt (mget partials 0 0)))))
                                  (sqrt (- (mget partials 1 1)
                                               (** (/ (mget partials 1 0)
                                                      (sqrt (mget partials 0 0)))
                                                   2))))
                               2)))]
    [[J-0-0 J-1-0 J-2-0]
     [J-1-0 J-1-1 J-2-1]
     [J-2-0 J-2-1 J-2-2]]))

(defn step [grad J]
  (let [ng-0 (/ (- (/ (mget grad 0) (mget J 0 0))
                   (* (mget J 2 0)
                      (/ (- (mget grad 2)
                            (* (mget J 2 0)
                               (/ (mget grad 0)
                                  (mget J 0 0)))
                            (* (mget J 2 1)
                               (/ (- (mget grad 1)
                                     (* (mget J 1 0)
                                        (/ (mget grad 0)
                                           (mget J 0 0))))
                                  (mget J 1 1))))
                         (mget J 2 2)))
                   (* (mget J 1 0)
                      (/ (- (mget grad 1)
                            (* (mget J 1 0)
                               (/ (mget grad 0)
                                  (mget J 0 0))))
                         (mget J 1 1))))
                (mget J 0 0))
        ng-1 (/ (- (/ (- (mget grad 1)
                         (* (mget J 1 0)
                            (/ (mget grad 0)
                               (mget J 0 0))))
                      (mget J 1 1))
                   (* (mget J 2 1)
                      (/ (- (mget grad 2)
                            (* (mget J 2 0)
                               (/ (mget grad 0)
                                  (mget J 0 0)))
                            (* (mget J 2 1)
                               (/ (- (mget grad 1)
                                     (* (mget J 1 0)
                                        (/ (mget grad 0)
                                           (mget J 0 0))))
                                  (mget J 1 1))))
                         (mget J 2 2))))
                (mget J 1 1))
        ng-2 (/ (/ (- (mget grad 2)
                      (* (mget J 2 0)
                         (/ (mget grad 0)
                            (mget J 0 0)))
                      (* (mget J 2 1)
                         (/ (- (mget grad 1)
                               (* (mget J 1 0)
                                  (/ (mget grad 0)
                                     (mget J 0 0))))
                            (mget J 1 1))))
                   (mget J 2 2))
                (mget J 2 2))]
    [ng-0 ng-1 ng-2]))

(defn convergent? [step A iterations]
  (or (and (< (norm step) 0.1)
           (< (squared-norm A) 0.1))
      (> iterations max-iterations)))

(defn trilaterate-vehicle [satellites start-point]
  (let [solution (loop [vehicle-position start-point
                        iterations 0]
                   (let [satellite-times (mmul satellites (transpose [[0 1 0 0 0]]))
                         satellites-cartesian (mmul satellites (transpose [[0 0 1 0 0]
                                                                           [0 0 0 1 0]
                                                                           [0 0 0 0 1]]))
                         sat-range (range 0 (dec (count satellites)))
                         N (satellite-distances vehicle-position satellites-cartesian)
                         A (A N satellite-times sat-range)
                         coordinates (gradient-A satellites-cartesian N vehicle-position sat-range)
                         grad (->> (map * A coordinates)
                                   (apply +))
                         partials (partial-derivatives satellites-cartesian
                                                       coordinates
                                                       N
                                                       vehicle-position
                                                       sat-range
                                                       A)
                         J (jacobian partials)
                         step (step grad J)
                         next-guess (- vehicle-position step)]
                     (if (convergent? step A iterations)
                       {:path-point next-guess
                        :iterations iterations}
                       (recur next-guess
                              (inc iterations)))))
        path-point (:path-point solution)]
    path-point))

(defn run [data input]
  (read-constants! data)
  (let [vehicle-groups (group-by-index-change input)
        start-time (nth (ffirst vehicle-groups) 1)
        out (loop [i 0
                   position b12
                   satellites (first vehicle-groups)
                   time (+ 0.05 (nth (first satellites) 1))
                   points []]
              (let [theta (/ (* @tau time) @s)
                    rotation (rotation-matrix theta)
                    start-point (mmul rotation position)
                    trilateration (trilaterate-vehicle satellites start-point)
                    time (+ (nth (first satellites) 1) 0.05)
                    theta (/ (* (- @tau) time) @s)
                    rotation (rotation-matrix theta)
                    path-point (mmul rotation trilateration)
                    path-rad (cartesian->rad [time] [path-point])
                    path-dms (radians->dms path-rad)
                    path-rounded (round-seconds path-dms)]
                (if (= (inc i) (count vehicle-groups))
                  (conj points path-rounded)
                  (let [next-satellites  (nth vehicle-groups (inc i))]
                    (recur (inc i)
                           trilateration
                           next-satellites
                           (+ 0.05 (- (nth (first next-satellites) 1) time))
                           (conj points path-rounded))))))]
    (apply concat out)))

(defn -main [& args]
  (let [data (-> "data.dat"
                 file->matrix
                 (get-column 0))]
    (->> (stdin->matrix)
         (run data)
         matrix->stdout)
    :ok))
