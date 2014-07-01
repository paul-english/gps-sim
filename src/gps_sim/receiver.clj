(ns gps-sim.receiver
  (:gen-class)
  (:refer-clojure :exclude [* - / + ==])
  (:use clojure.core.matrix
        clojure.core.matrix.operators)
  (:require [schema.macros :as sm]
            [schema.coerce :as coerce]
            ;; [clojure.core.matrix.linear :refer [norm]] ;; this was
            ;; just recently added
            [gps-sim.constants :refer [read-constants! tau s c]]
            [gps-sim.utils.coordinates :refer [dms->radians
                                               rad->cartesian
                                               cartesian->rad
                                               radians->dms]]
            [gps-sim.utils.io :refer [file->matrix stdin->matrix]]
            [gps-sim.utils.matrix :refer [rotation-matrix norm]]
            [gps-sim.utils.schemas :refer [DMSCoordinateList
                                           DataFile
                                           CartesianSatelliteList
                                           parse-data
                                           parse-dms-list
                                           parse-cartesian-satellite-list
                                           parse-cartesian-list]]
            [gps-sim.utils.numeric :refer [round-places]]
            [gps-sim.utils.sequences :refer [partition-when]]))

;; TODO set this for each different jar & remove the with-precision calls
(set! *warn-on-reflection* true)
(set! *unchecked-math* true)
;;(set! *math-context* (java.math.MathContext. 20))

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

(sm/defn run :- DMSCoordinateList
  [data :- DataFile
   input :- CartesianSatelliteList]
  (read-constants! data)
  (let [path-satellites (group-by-index-change input)
        b12 (->> [[0M 40 45 55.0M 1 111 50 58.0M -1 1372.0M]]
                 dms->radians
                 rad->cartesian
                 first)
        max-iterations 30
        _ (println "b12" b12)
        _ (println "# sat-paths" (count path-satellites) (range (count path-satellites)))

        ;; TODO refactor this into a separate function
        out (for [epoch (range (count path-satellites))]
              (do
                (println "Epoch:" epoch)
                (let [satellites (nth path-satellites epoch)
                      start-time (nth (first satellites) 1)
                      time-diff 0.05
                      path-time (+ start-time time-diff)
                      theta (/ (* @tau path-time) @s)
                      rotation (rotation-matrix (bigdec theta))
                      path-point (loop [vehicle-position (mmul rotation b12)
                                        iterations 0]
                                   (println "----vehicle-position-----" vehicle-position)
                                   (println "----iterations-----" iterations)
                                   (let [
                                         ;;_ (println "first sat" (first satellites))
                                         satellite-times (mmul satellites (transpose [[0 1 0 0 0]]))
                                         ;;_ (println "sat-times" satellite-times)
                                         satellites-cartesian (mmul satellites (transpose [[0 0 1 0 0]
                                                                                           [0 0 0 1 0]
                                                                                           [0 0 0 0 1]]))
                                         ;;_ (println "sat-cart" satellites-cartesian)
                                         N (map #(distance % vehicle-position) satellites-cartesian)
                                         ;;_ (println "N" N)

                                         lower-sat-range (range 0 (dec (count satellites)))

                                         A (mapcat #(- (nth N (inc %))
                                                       (nth N %)
                                                       (* @c
                                                          (- (nth satellite-times %)
                                                             (nth satellite-times (inc %)))))
                                                   lower-sat-range)
                                         ;;_ (println "A" A)

                                         coordinates (map #(let [current-sat (nth satellites-cartesian %)
                                                                 next-sat (nth satellites-cartesian (inc %))
                                                                 current-N (nth N %)
                                                                 next-N (nth N (inc %))]
                                                             (- (/ (- current-sat vehicle-position)
                                                                   current-N)
                                                                (/ (- next-sat vehicle-position)
                                                                   next-N)))
                                                          lower-sat-range)
                                         ;;_ (println "X Y Z")
                                         ;;_ (pm coordinates)

                                         ;; TODO need a better name
                                         f-min (->> A
                                                    (map #(** % 2))
                                                    (apply +))
                                         ;;_ (println "f-min" f-min)


                                         grad (->> (map * A coordinates)
                                                   (apply +))
                                         ;;_ (println "grad" grad)

                                         ;; second-partials
                                         ;; TODO get ad working with this shit, I don't want all of
                                         ;; these retarded functions
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
                                                             lower-sat-range))
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
                                                             lower-sat-range))
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
                                                             lower-sat-range))
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
                                                             lower-sat-range))
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
                                                             lower-sat-range))
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
                                                             lower-sat-range))

                                         second-partials [[dx-dx dy-dx dz-dx]
                                                          [dx-dy dy-dy dz-dy]
                                                          [dx-dz dy-dz dz-dz]]
                                         ;;_ (println "second-partials")
                                         ;;_ (pm second-partials)

                                         ;; TODO this isn't J, it's something different...
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
                                         ;;_ (println "J")
                                         ;;_ (pm J)


                                         ;; TODO shit balls, what is this?
                                         ;; order matters here
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
                                         ;;_ (println "next-guess" next-guess)

                                         new-vehicle-position (- vehicle-position
                                                                 next-guess)
                                         _ (println "new-vehicle-position" new-vehicle-position)

                                         next-norm (norm next-guess)
                                         _ (println "next-norm" next-norm)]
                                     (if (or (and (< next-norm 0.1)
                                                  (< f-min 0.1))
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
                      ;; TODO refactor these fns
                      column-map (fn [f i m]
                                   (->> (get-column m i)
                                        (emap f)
                                        (set-column m i)))
                      round-seconds (fn [A]
                                      (letfn [(round [e]
                                                (->> e
                                                     (round-places 0)
                                                     bigdec))]
                                        (->> A
                                             (column-map round 3)
                                             (column-map round 7))))
                      path-rounded (round-seconds path-dms)]
                  (println "path-rounded" path-rounded)
                  path-rounded)))]
    (apply concat out)))

(defn -main [& args]
  (let [data (-> "data.dat" file->matrix (get-column 0))
        input (stdin->matrix)]
    ;;(println "--" input)
    ;;(println "--" (parse-data data))
    ;;(println "--" (parse-cartesian-list input))
    (run (parse-data data)
         (parse-cartesian-satellite-list input))
    :ok
    )
  )
