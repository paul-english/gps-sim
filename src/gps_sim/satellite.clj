(ns gps-sim.satellite
  (:refer-clojure :exclude [* - + == /])
  (:use clojure.core.matrix
        clojure.core.matrix.operators)
  (:require [schema.core :as s]
            [schema.coerce :as coerce]
            [gps-sim.constants :refer [read-constants! R s c tau]]
            [gps-sim.utils.coordinates :refer [dms->radians rad->cartesian cartesian->rad]]
            [gps-sim.utils.io :refer [file->matrix stdin->matrix]]
            [gps-sim.utils.matrix :refer [join-1 rotation-matrix]]
            [gps-sim.utils.schemas :refer [DMSCoordinateList
                                           CartesianCoordinate
                                           CartesianCoordinateList
                                           DataFile
                                           CartesianSatelliteCoordinate
                                           CartesianSatelliteList
                                           SatelliteList
                                           RadCoordinateList
                                           parse-data
                                           parse-dms-list
                                           parse-cartesian-list
                                           parse-cartesian-satellite-list]]))

(s/defn rotate-coordinates :- CartesianCoordinateList
  [times :- [BigDecimal]
   coordinates :- CartesianCoordinateList]
  ;;(println "- rotate-coordinates" coordinates)
  (let [theta (with-precision 20 (/ times @s))
        ;;_ (println "    -> theta" theta)
        rotations (emap rotation-matrix theta)
        ;;_ (println "    -> rotations" rotations)
        rotated (map #(mmul %1 %2)
                     rotations
                     coordinates)]
    ;;(println "    -> rotated" rotated)
    (parse-cartesian-list rotated)))

(s/defn above-horizon? :- Boolean
  "To determine if a satellite is above the horizon,
we can project the satellite vector onto the vehicle
position vector. Then if the projection is greater
then the magnitude of the vehicle vector we know it's
above the horizon. This can be simplified as follows,

proj(x_s, x_V) = \\frac{x_s \\cdot x_V}{|| x_V ||} &> || x_V || \\\\
x_s \\cdot x_V &> || x_V ||^2
x_s \\cdot x_V &> x_V \\cdot x_V"
  [vehicle :- CartesianCoordinate
   satellite :- CartesianSatelliteCoordinate]
  ;;(println "- above-horizon?")
  ;;(println "    -> vehicle" vehicle)
  ;;(println "    -> satellite" satellite)
  ;;(println "    -> s * v" (dot (drop 2 satellite) vehicle))
  ;;(println "    -> v * v" (dot vehicle vehicle))
  ;;(println "    -> >" (> (dot (drop 2 satellite) vehicle) (dot vehicle vehicle)))
  (> (dot (drop 2 satellite) vehicle)
     (dot vehicle vehicle)))

(s/defn satellite-location :- CartesianCoordinateList
  [satellites :- SatelliteList
   t :- [BigDecimal]]
  (println "- satellite-location")
  (let [u (mmul satellites (transpose [[0 1 0 0 0 0 0 0 0 0]
                                       [0 0 1 0 0 0 0 0 0 0]
                                       [0 0 0 1 0 0 0 0 0 0]]))
        _ (println "    -> u" (first (transpose u)))
        v (mmul satellites (transpose [[0 0 0 0 1 0 0 0 0 0]
                                       [0 0 0 0 0 1 0 0 0 0]
                                       [0 0 0 0 0 0 1 0 0 0]]))
        _ (println "    -> v" (first (transpose v)))
        period (get-column satellites 7)
        _ (println "    -> period" (first period))
        h (get-column satellites 8)
        _ (println "    -> h" (first h))
        phase (get-column satellites 9)
        _ (println "    -> phase" (first phase))
        theta (with-precision 20 (+ (/ (* @tau t) period)
                                phase))
        _ (println "    -> theta" (first theta))
        coordinates (* (+ @R h)
                       (+ (* (transpose u) (cos theta))
                          (* (transpose v) (sin theta))))]
    (println "    -> coordinates" (first (transpose coordinates)))
    (parse-cartesian-list (transpose coordinates))))

;; TODO test & better name, schema
(defn satellite-time [{:keys [pseudorange vehicle-coordinates new-coordinates]}]
  ;; [[index t x y z]]
  ;;   - calc f_x = || x_v - x_s || - squared-norm
  (println "- satellite-times" vehicle-coordinates (first new-coordinates))
  (println "    -> pseudorange" (first pseudorange))
  (- (map #(dot vehicle-coordinates %) new-coordinates)
     (** pseudorange 2)))

;; TODO test & schema
(defn gradient [{:keys [pseudorange satellites
                        satellite-times new-coordinates
                        vehicle-coordinates]}]
  ;;(println "- gradient")
  (let [u (mmul satellites (transpose [[0 1 0 0 0 0 0 0 0 0]
                                       [0 0 1 0 0 0 0 0 0 0]
                                       [0 0 0 1 0 0 0 0 0 0]]))
        v (mmul satellites (transpose [[0 0 0 0 1 0 0 0 0 0]
                                       [0 0 0 0 0 1 0 0 0 0]
                                       [0 0 0 0 0 0 1 0 0 0]]))
        periods (get-column satellites 7)
        heights (get-column satellites 8)
        phases (get-column satellites 9)
        theta (with-precision 20 (+ (/ (* @tau satellite-times) periods)
                                phases))
        location-gradient (with-precision 20 (* (/ @tau periods)
                                                (+ @R heights)
                                                (+ (* (- (transpose u)) (sin theta))
                                                   (* (transpose u) (cos theta)))))
        coordinate-diffs (- new-coordinates
                            (repeat (count satellites) vehicle-coordinates))
        out (* 2 (+ (* @c pseudorange)
                    (map dot
                         coordinate-diffs
                         (transpose location-gradient))))]
    ;;(println "    -> theta" (shape theta))
    ;;(println "    -> u" (shape u))
    ;;(println "    -> coordinate-diffs" (shape coordinate-diffs))
    ;;(println "    -> loc-grad" (shape location-gradient))
    ;;(println "    -> grad out" (first out))
    out))

;; TODO test & schema
;; [t_s]
(defn newtons [{:keys [satellite-times] :as params}]
  (println "- newtons")
  (let [step (/ (satellite-time params) (gradient params))
        _ (println "    -> sat-times" (first satellite-times))
        _ (println "    -> f(x)" (first (satellite-time params)))
        _ (println "    -> f_prime(x)" (first (gradient params)))
        _ (println "    -> step" (first step))
        out (- satellite-times step)]
    (println "    -> newtons out" (first out))
    out))

(defn convergent? [{:keys [error steps max-steps tolerance] :as params}]
  (or (< error tolerance)
      (> steps max-steps)))

;; TODO test & schema
(defn next-guess [{:keys [steps satellite-coordinates
                          satellite-times vehicle-times
                          satellites] :as params}]
  (println "- next-guess")
  (let [new-coordinates (satellite-location satellites
                                            (map bigdec satellite-times))
        _ (println "    -> new-coordinates" (first new-coordinates))
        new-times (newtons (assoc params :new-coordinates new-coordinates))
        _ (println "    -> new-times" (first new-times))
        new-pseudorange (- vehicle-times new-times)
        _ (println "    -> new-pseudorange" (first new-pseudorange))
        error (distance satellite-times new-times)]
    (assoc params
      :satellite-times new-times
      :satellite-coordinates new-coordinates
      :pseudorange new-pseudorange
      :error error
      :steps (inc steps))))

(defn solve [start]
  (println "---------------------")
  (println "- solve" (keys start))
  (first (drop-while #(not (convergent? %))
                     (iterate #(next-guess %) start))))

(s/defn run :- CartesianSatelliteList
  [data :- DataFile
   input :- DMSCoordinateList]
  (read-constants! data)
  (println "---------------------")
  (println "- run")
  (let [satellites (->> data
                        (drop 4)
                        (partition 9))
        indexed-satellites (join-1 (transpose [(range 0 (count satellites))])
                                   satellites)
        input-radians (dms->radians input)             ; t, psi, lambda, h
        input-cartesian (rad->cartesian input-radians) ; x, y, z
        vehicles (rotate-coordinates (get-column input 0)
                                     input-cartesian)]
    (println "#vehicles" (count vehicles))
    (doall
     (mapcat (fn [[time psi lambda h] vehicle]
               (println "    -> mapcat-fn")
               (let [times (repeat (count satellites) time)
                     _ (println "    -> times" (first times))
                     _ (println "    -> indexed-satellites" (first indexed-satellites))
                     satellite-coordinates (satellite-location indexed-satellites times)
                     _ (println "    -> sat-coordinates" (first satellite-coordinates))
                     _ (println "    -> vehicle" vehicle)
                     difference (- (repeat (count satellites) vehicle)
                                   satellite-coordinates)
                     _ (println "    -> difference" (first difference))
                     pseudorange (map distance difference difference)
                     _ (println "    -> pseudorange" (first pseudorange))
                     satellite-times (- time (/ pseudorange
                                                @c))
                     _ (println "    -> satellite-times" (first satellite-times))

                     ;; calc x(t_s) using initial vehicle time
                     ;; calc squared-norm = || x_v - x_s ||^2
                     ;; calc t_s = t_v - (sqrt(squared-norm) / c)
                     solution (solve {:satellites indexed-satellites
                                      :satellite-coordinates satellite-coordinates
                                      :pseudorange pseudorange
                                      :vehicle-times times
                                      :vehicle-coordinates vehicle
                                      :satellite-times satellite-times
                                      :tolerance (/ 0.01 @c)
                                      :error 1
                                      :max-steps 10
                                      :steps 0})
                     _ (println "    -> solution" (keys solution))
                     _ (println "    -> sat-coordinates" (first (:satellite-coordinates solution)))
                     _ (println "    -> sat-times" (first (:satellite-times solution)))
                     solved-satellites (join-1 (transpose [(range 0 (count satellites))
                                                           (:satellite-times solution)])
                                               (:satellite-coordinates solution))
                     _ (println "    -> solved-sats" (first solved-satellites))
                     ]
                 (filter (partial above-horizon? vehicle)
                         (parse-cartesian-satellite-list solved-satellites))))
             input-radians
             vehicles))))

(defn -main [& args]
  (let [data (-> "data.dat" file->matrix (get-column 0))
        input (stdin->matrix)]
    (run (parse-data data)
         (parse-dms-list input))
    ;; TODO output of run to stdout
    :ok))
