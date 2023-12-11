(ns aoclj.2023.day11
  (:require [aoclj.tools :refer [lines get-input filter-columns filter-rows find-with-coords]]))

;; Ultra-smart solution; tried with memoize first but is faster without
(defn calc-d' [to-expand age a b]
  (if (= a b) 0
      (+ (if (get to-expand b) age 1)
         (calc-d' to-expand age a (dec b)))))

;; Repeated filtering is even faster than "smart" solution
(defn calc-d [to-expand age a b]
  (let [d0 (- b a)
        gaps (count (filter #(< a % b) (keys to-expand)))]
    (+ d0 (* gaps (dec age)))))

(defn manhattan-in-universe [compressed age]
  (fn [[x1 y1] [x2 y2]]
    (+ (apply (partial calc-d (:cols compressed) age) (sort [x1 x2]))
       (apply (partial calc-d (:rows compressed) age) (sort [y1 y2])))))

;; Any faster solutions? Takes half a second... :(
(defn solve [age input]
  (let [input' (lines input)
        has-no-galaxy (fn [line] (every? #(= \. %) line))
        universe {:cols (into {} (filter-columns has-no-galaxy input'))
                  :rows (into {} (filter-rows has-no-galaxy input'))}
        galaxies (map first (find-with-coords #(not= \. %) identity input'))
        pairs (set (for [a galaxies
                         b galaxies
                         :when (not= a b)]
                     (sort-by (juxt first second) [a b])))]
    (->> pairs
         (map (partial apply (manhattan-in-universe universe age)))
         (apply +))))

(def part-1 (partial solve 2))
(def part-2 (partial solve 1000000))

(comment
  (part-1 "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
")
  (time (part-1 (get-input 2023 11)))
  (time (part-2 (get-input 2023 11)))
  ,)
