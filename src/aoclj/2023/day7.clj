(ns aoclj.2023.day7
  (:require [aoclj.tools :refer [lines get-input contains-all]]
            [clojure.string :as str]))

    (defn freqs->type [freqs]
      (condp #(contains-all %1 %2) (vals freqs)
        [5]   6
        [4]   5
        [3 2] 4
        [3]   3
        [2 2] 2
        [2]   1
        0))

    (defn parse-value [freqs value-for-j [hand bet]]
      (let [hand' (-> (str/replace hand "T" "a")
                      (str/replace "J" value-for-j)
                      (str/replace "Q" "c")
                      (str/replace "K" "d")
                      (str/replace "A" "e"))]
        {:hand hand
         :strength hand'
         :type (freqs->type (freqs hand))
         :bet (parse-long bet)}))

    (defn solve [freqs value-for-j input]
      (->> (lines input)
           (map #(str/split % #" "))
           (map (partial parse-value freqs value-for-j))
           (sort-by (juxt :type :strength))
           (map-indexed #(* (inc %1) (:bet %2)))
           (apply +)))

    (def part-1 (partial #'solve frequencies "b"))

    (defn optimize-freqs [cards]
      (let [fs (frequencies cards)]
        (if-let [js (get fs \J)]
          (let [[strongest-key] (last (sort-by second (dissoc fs \J)))
                optimized (if strongest-key (update fs strongest-key + js) fs)]
            (if-not strongest-key
              optimized
              (dissoc optimized \J)))
          fs)))

    (def part-2 (partial #'solve optimize-freqs "1"))

(comment
  (optimize-freqs "KTJJT")
  (optimize-freqs "JJJJJ")
  ,)


#_(let [example "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"]
    (#'part-1 example)
    (#'part-2 example))

(comment
  (prn (#'part-1 (get-input 2023 7)))
  (prn (#'part-2 (get-input 2023 7)))
  ,)
