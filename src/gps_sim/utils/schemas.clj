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
                 BigDecimal (coerce/safe #(BigDecimal/valueOf %))
                 Boolean coerce/string->boolean}]
  (defn data-coercion-matcher
    "A custom matcher"
    [schema]
    (or (coercions schema)
        (coerce/keyword-enum-matcher schema))))

(defn BoundedReal [lower upper]
  (s/both BigDecimal
          (s/pred #(>= % lower) 'lower-bounds)
          (s/pred #(<= % upper) 'upper-bounds)))

(defn BoundedInt [lower upper]
  (s/both Long
          (s/pred #(>= % lower) 'lower-bounds)
          (s/pred #(<= % upper) 'upper-bounds)))

;; TODO proper bounds on start,end
(def DMSPath [(s/one BigDecimal "start")
              (s/one BigDecimal "end")
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

(def DMSCoordinate [(s/one BigDecimal "time instance")
                    (s/one Long "psi_d")
                    (s/one (BoundedInt 0 60) "psi_m")
                    (s/one (BoundedReal 0 60) "psi_s")
                    (s/one (s/enum 1 -1) "NS")
                    (s/one Long "lambda_d")
                    (s/one (BoundedInt 0 60) "lambda_m")
                    (s/one (BoundedReal 0 60) "lambda_s")
                    (s/one (s/enum 1 -1) "EW")
                    (s/one (BoundedReal -10000 10000000) "height")])

(def Satellite [(s/one BigDecimal "u_1")
                (s/one BigDecimal "u_2")
                (s/one BigDecimal "u_3")
                (s/one BigDecimal "v_1")
                (s/one BigDecimal "v_2")
                (s/one BigDecimal "v_3")
                (s/one BigDecimal "periodicity")
                (s/one BigDecimal "altitude")
                (s/one BigDecimal "phase")])

(def Constants {:pi BigDecimal
                :c BigDecimal
                :R BigDecimal
                :s BigDecimal})

(def DataFile [BigDecimal])

(def VehicleInput [(s/one DMSPath "input")])
(def VehicleOutput [DMSCoordinate])

(def SatelliteInput [DMSCoordinate])
(def SatelliteOutput [])

(def ReceiverInput [])
(def ReceiverOutput [])
