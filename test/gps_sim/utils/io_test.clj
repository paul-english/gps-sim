(ns gps-sim.utils.io-test
  (:use midje.sweet
        gps-sim.utils.io)
  (:require [clojure.core.matrix :refer [matrix?]]
            [clojure.java.io :refer [reader]]))

(with-state-changes [(around :facts (schema.macros/with-fn-validation ?form))]
  (facts "IO"
    (fact "tokenize"
      (tokenize "1 2 3 4") => ["1" "2" "3" "4"])

    (fact "map-read"
      (map-read ["1" "2" "3"]) => [1 2 3]
      (map-read ["one" "2" "3"]) => ['one 2 3])

    (fact "drop-end-newline"
      (drop-end-newline [[1] [""]]) => [[1]])

    (fact "data->matrix"
      (let [lines (line-seq (reader "data/bm.dat"))
            m (data->matrix lines)]
        m => [[102123.0 112000.0 90 40 48 44 1 111 48 40 -1 2377]]
        (matrix? m) => true))

    (fact "file->matrix"
      (let [m (file->matrix "data/bm.dat")]
        m => [[102123.0 112000.0 90 40 48 44 1 111 48 40 -1 2377]]
        (matrix? m) => true))

    (fact "stdin->matrix"
      (with-in-str "1 2 3\n4 5 6"
        (let [m (stdin->matrix)]
          m => [[1 2 3] [4 5 6]]
          (matrix? m) => true)))))
