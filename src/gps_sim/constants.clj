(ns gps-sim.constants
  (:require [schema.core :as s]
            [gps-sim.utils.schemas :refer [Constants DataFile]]))

;; Sensible defaults
(def pi (atom Math/PI))
(def tau (atom (* 2 @pi)))
(def c (atom 299792458M))
(def R (atom 6367444.5M))
(def s (atom 86164.09M))

(s/defn set-constants! [constants :- Constants]
  (reset! pi (:pi constants))
  (reset! tau (* 2 @pi))
  (reset! c (:c constants))
  (reset! R (:R constants))
  (reset! s (:s constants)))

(s/defn read-constants! [data :- DataFile]
  (set-constants! (zipmap [:pi :c :R :s]
                          data)))
