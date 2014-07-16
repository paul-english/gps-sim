(ns gps-sim.vehicle
  (:gen-class)
  (:refer-clojure :exclude [* - / + ==])
  (:use clojure.core.matrix
        clojure.core.matrix.operators)
  (:require [gps-sim.utils.io :refer [stdin->matrix matrix->stdout]]
            [gps-sim.utils.numeric :refer [round-places num-decimals] :as num]
            [gps-sim.utils.sequences :refer [zipjuxt]]
            [gps-sim.utils.coordinates :refer [dms->radians radians->dms]]
            [gps-sim.utils.matrix :refer [lerp]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def b12 [40 45 55 1 111 50 58 -1 1372.0])

(defn round-row [row]
  ((zipjuxt (partial round-places 2)
            num/round num/round (partial round-places 1) num/round ;; lat
            num/round num/round (partial round-places 1) num/round ;; lng
            (partial round-places 2))
   row))

(defn round-output-interpolation [interpolation]
  (->> interpolation
       rows
       (map round-row)))

(defn run [input]
  (let [start-time (mget input 0 0)
        end-time (mget input 0 1)
        steps (mget input 0 2)
        end-point (->> input first (drop 3))
        step-size (if (pos? steps) (double (/ 1 steps)) 1)]
    (-> (matrix [(concat [start-time] b12)
                 (concat [end-time] end-point)])
        dms->radians
        (lerp step-size)
        round-output-interpolation
        radians->dms)))

(defn -main [& args]
  (-> (stdin->matrix)
      run
      matrix->stdout)
  :ok)
