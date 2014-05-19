(ns gps-sim.utils.sequences)

(defn zipjuxt
  "Similar to juxt, only applies each function to
the arg at the corresponding index.

((zipjuxt a b c) x) => [(a (nth x 0)) (b (nth x 1)) (c (nth x 2))] "
  [& fns]
  (fn [coll]
    (map (fn [[f v]]
           (f v))
         (map vector fns coll))))
