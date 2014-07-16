(ns gps-sim.utils.schemas
  (:require [schema.core :as s]
            [schema.coerce :as coerce]))

(defn read-obj [in]
  (if (= (type in) String)
    (coerce/edn-read-string in)
    in))

(let [coercions {s/Keyword coerce/string->keyword
                 s/Num (coerce/safe read-obj)
                 clojure.lang.Keyword coerce/string->keyword
                 s/Int (coerce/safe #(coerce/safe-long-cast (read-obj %)))
                 Long (coerce/safe #(coerce/safe-long-cast (read-obj %)))
                 Double (coerce/safe #(Double/parseDouble %))
                 Boolean coerce/string->boolean}]
  (defn data-coercion-matcher
    "A custom matcher"
    [schema]
    (or (coercions schema)
        (coerce/keyword-enum-matcher schema))))

(defn BoundedReal [lower upper]
  (s/both Double
          (s/pred #(>= % lower) 'lower-bounds)
          (s/pred #(<= % upper) 'upper-bounds)))

(defn BoundedInt [lower upper]
  (s/both Long
          (s/pred #(>= % lower) 'lower-bounds)
          (s/pred #(<= % upper) 'upper-bounds)))

;; TODO proper bounds on start,end
(def DMSPath [(s/one Double "start")
              (s/one Double "end")
              (s/one Long "steps")
              (s/one Long "lat_d")
              (s/one (BoundedInt 0 60) "lat_m")
              (s/one (BoundedInt 0 60) "lat_s")
              (s/one (s/enum 1 -1) "NS")
              (s/one Long "lng_d")
              (s/one (BoundedInt 0 60) "lng_m")
              (s/one (BoundedInt 0 60) "lng_s")
              (s/one (s/enum 1 -1) "EW")
              (s/one (BoundedInt -10000 10000000) "height")])

(def DMSCoordinate [(s/one Double "time instance")
                    (s/one Long "psi_d")
                    (s/one (BoundedInt 0 60) "psi_m")
                    (s/one (BoundedReal 0 60) "psi_s")
                    (s/one (s/enum 1 -1) "NS")
                    (s/one Long "lambda_d")
                    (s/one (BoundedInt 0 60) "lambda_m")
                    (s/one (BoundedReal 0 60) "lambda_s")
                    (s/one (s/enum 1 -1) "EW")
                    (s/one (BoundedReal -10000 10000000) "height")])

(def RadCoordinate [(s/one Double "time")
                    (s/one Double "psi")
                    (s/one Double "lambda")
                    (s/one (BoundedReal -10000 10000000) "height")])

(def Satellite [(s/one s/Num "index")
                (s/one Double "u_1")
                (s/one Double "u_2")
                (s/one Double "u_3")
                (s/one Double "v_1")
                (s/one Double "v_2")
                (s/one Double "v_3")
                (s/one Double "periodicity")
                (s/one Double "altitude")
                (s/one Double "phase")])

(def CartesianCoordinate [(s/one Double "x")
                          (s/one Double "y")
                          (s/one Double "z")])

(def CartesianSatelliteCoordinate [(s/one (BoundedInt 0 24) "satellite_index")
                                   (s/one Double "time")
                                   (s/one Double "x")
                                   (s/one Double "y")
                                   (s/one Double "z")])

(def Constants {:pi Double
                :c Double
                :R Double
                :s Double})

(def DataFile [Double])
(def VehicleInput [(s/one DMSPath "input")])
(def DMSCoordinateList [DMSCoordinate])
(def CartesianCoordinateList [CartesianCoordinate])
(def CartesianSatelliteList [CartesianSatelliteCoordinate])
(def SatelliteList [Satellite])
(def RadCoordinateList [RadCoordinate])

(def parse-vehicle-input (coerce/coercer VehicleInput data-coercion-matcher))
(def parse-data (coerce/coercer DataFile data-coercion-matcher))
(def parse-dms-list (coerce/coercer DMSCoordinateList data-coercion-matcher))
(def parse-cartesian-list (coerce/coercer CartesianCoordinateList data-coercion-matcher))
(def parse-cartesian-satellite-list (coerce/coercer CartesianSatelliteList data-coercion-matcher))
(def parse-satellite-list (coerce/coercer SatelliteList data-coercion-matcher))
(def parse-rad-list (coerce/coercer RadCoordinateList data-coercion-matcher))
