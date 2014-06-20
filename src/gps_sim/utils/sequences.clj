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

(defn take-to-first
  "Returns a lazy sequence of successive items from coll up to
  and including the point at which it (pred item) returns true.
  pred must be free of side-effects."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
       (if-not (pred (first s))
         (cons (first s) (take-to-first pred (rest s)))
         (list (first s))))))

(defn partition-when
  "Applies f to each value in coll, splitting it each time f returns
   true. Returns a lazy seq of lazy seqs."
  [f coll]
  (when-let [s (seq coll)]
    (let [run (take-to-first #(f %) s)
          res (drop (count run) s)]
      (lazy-seq
       (cons run (partition-when f res))))))
