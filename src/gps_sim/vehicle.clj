(ns gps-sim.vehicle
  (:refer-clojure :exclude [* - / + ==])
  (:use clojure.core.matrix
        clojure.core.matrix.operators)
  (:require [schema.core :as s]
            [schema.coerce :as coerce]
            [gps-sim.utils.io :refer [stdin->matrix]]
            [gps-sim.utils.numeric :refer [round-places num-decimals] :as num]
            [gps-sim.utils.sequences :refer [zipjuxt]]
            [gps-sim.utils.angles :refer [dms->radians radians->dms]]
            [gps-sim.utils.schemas :refer [VehicleInput DMSCoordinateList parse-vehicle-input parse-dms-list]]
            [gps-sim.utils.matrix :refer [lerp]]))

(s/defn run :- DMSCoordinateList
  [input :- VehicleInput]
  (let [start-time (mget input 0 0)
        end-time (mget input 0 1)
        steps (mget input 0 2)
        step-size (double (/ 1 steps))
        b12 [40 45 55 1 111 50 58 -1 1372.0]
        end-point (->> input first (drop 3))
        output-bounds (matrix [(concat [start-time] b12)
                               (concat [end-time] end-point)])
        radian-bounds (dms->radians output-bounds)
        interpolation (lerp radian-bounds step-size)
        ;; TODO this is probably not needed with coercion
        rounded (map (fn [r]
                       ;;(println "-" (count r))
                       ((zipjuxt (partial round-places 2)
                                 num/round num/round (partial round-places 1) num/round ;; lat
                                 num/round num/round (partial round-places 1) num/round ;; lng
                                 (partial round-places 2))
                        r
                        ))
                     (rows interpolation))
        output (radians->dms rounded)]
    ;;(println "---- run")
    ;;(println "time" start-time end-time)
    ;;(println "step-size" step-size)
    ;;(println "output-bounds")
    ;;(pm output-bounds)
    ;;(println "----")
    ;;(pm (take 5 output))
    ;;(pm (last output))
    (parse-dms-list output)))

(defn -main [& args]
  (let [input (stdin->matrix)]
    (run (parse-vehicle-input input))
    ;; TODO (-> stdin->matrix partition-path matrix->stdout)
    :ok))
