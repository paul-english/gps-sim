(ns gps-sim.utils.io
  (:require [clojure.java.io :refer [reader]]
            [clojure.string :refer [trim split]]
            [clojure.core.matrix :refer [matrix]]))

(defn tokenize [str]
  (split str #" "))

(defn map-read [row]
  (map #(try (read-string %)
             (catch Exception e %))
       row))

(defn drop-end-newline [coll]
  (if (= (last coll) [""])
    (drop-last coll)
    coll))

(defn data->matrix [lines]
  (let [m (count lines)]
    (->> lines
         (map (comp map-read tokenize trim))
         drop-end-newline
         matrix)))

(defn file->matrix [filename]
  (data->matrix (line-seq (reader filename))))

(defn stdin->matrix []
  (data->matrix (line-seq (java.io.BufferedReader. *in*))))

(defn matrix->stdout [m]
  (doseq [row m]
    (->> row
         (clojure.string/join " ")
         println)))
