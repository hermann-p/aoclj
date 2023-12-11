(ns aoclj.2023.day6
  (:require [aoclj.algorithms :refer [quadratic-roots]]
            [clojure.math :refer [floor ceil]]))

;; Now really, it's quicker to c&p than (-> lines re-seq...)

(def input [[35 213]
            [69 1168]
            [68 1086]
            [87 1248]])

(defn rng [t d]
  (let [[x1 x2] (quadratic-roots -1 t (- d))
        x1' (floor x1), x2' (ceil x2)]
    (-> (- x1' x2')
        Math/abs
        dec)))

(defn solve [input]
  (->> input
       (map (partial apply rng))
       (apply *)
       int))

(comment
  (#'rng 7 9)
  (#'rng 15 40)
  (#'rng 30 200)
  (#'solve [[7 9] [15 40] [30 200]])
  (#'solve input)
  ;; c&p input for part 1 was lucky coincidence, as modifying the input for part 2 manually saves even more time :D
  (#'solve [[35696887N 213116810861248N]])
  ,)
