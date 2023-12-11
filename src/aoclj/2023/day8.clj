(ns aoclj.2023.day8
  (:require [aoclj.tools :refer [get-input lines]]
            [clojure.pprint :refer [pprint]]))

(defn to-map [m [key & paths]]
  (assoc m key (vec paths)))

(defn lines->map [input-lines]
  (->> input-lines
       (map (partial re-seq #"[A-Z]{3}"))
       (reduce to-map {})))

#_(to-map {} ["AAA" "BBB" "CCC"])

(defn parse-input [input]
  (let [ls   (lines input)
        path (first ls)
        m    (drop 2 ls)
        _ (pprint m)]
    {:path  path
     :m     (lines->map m)}))

(defn part-1 [parsed-input]
  (let [path   (cycle   (:path parsed-input))
        loc    (fn [pos step] {:pos pos, :step step})
        start  (loc "AAA" 0)
        desert (:m  parsed-input)]
    (->> path
         (reduce (fn [{:keys [pos step]} dir]
             ;; (prn {:pos pos, :dir dir, :loc (get-in desert [pos])})
             (cond
               (or (nil? pos) (nil? dir)) (reduced {:error {:pos pos, :dir dir}})
               (= pos "ZZZ")  (reduced (loc pos step))
               (= \L dir)     (loc (get-in desert [pos 0]) (inc step))
               :else          (loc (get-in desert [pos 1]) (inc step))))
           start)
         )))

(comment
  (let [example "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"]
    (#'part-1 (parse-input example)))

  (let [example "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"]
    (#'part-1 (parse-input example)))

  (time (#'part-1 (parse-input (get-input 2023 8))))
  ,)

#_(get-input 2023 8)
