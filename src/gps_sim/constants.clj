(ns gps-sim.constants
  (:require [schema.macros :as sm]
            [gps-sim.utils.schemas :refer [Constants DataFile]]))

(def ^:dynamic pi (atom Math/PI))
(def ^:dynamic tau (atom (* 2 @pi)))
(def ^:dynamic c (atom 299792458M))
(def ^:dynamic R (atom 6367444.5M))
(def ^:dynamic s (atom 86164.09M))

(sm/defn set-constants! [constants :- Constants]
  (reset! pi (:pi constants))
  (reset! tau (* 2 @pi))
  (reset! c (:c constants))
  (reset! R (:R constants))
  (reset! s (:s constants)))

(sm/defn read-constants! [data :- DataFile]
  (set-constants! (zipmap [:pi :c :R :s]
                          data)))
