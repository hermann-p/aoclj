(ns aoclj.2023.day2
  (:require [aoclj.tools :refer [get-input lines]]
            [instaparse.core :as insta]))

;; I always wanted to try intaparse, although it's quite useless here :D

(def parse-line
  (insta/parser
   "<S> := Game <': '> Set(<'; '> Set)*
    Game := <'Game '> Number
    <Number> := #'[0-9]+'
    Set := Hand(<', '> Hand)*
    <Hand> := Number <' '> Color
    <Color> := 'blue' | 'red' | 'green'"
   {:output-format :hiccup}))

(defn line->game [line]
  (let [result     (parse-line line)
        [_ n-str]  (first result)
        hands      (map rest (rest result))]
    {:n     (parse-long n-str)
     :hands (map #(into {} (for [[n-str c-str] (partition 2 2 %)]
                             [(keyword c-str) (parse-long n-str)]))
                 hands)}))

(defn part-1 [input]
  (->> (lines input)
       (map line->game)
       (filter (fn [{:keys [hands]}]
                 (every?
                  (fn [{:keys [red green blue] :or {red 0, green 0, blue 0}}]
                    (and (<= red 12)
                         (<= green 13)
                         (<= blue 14)))
                  hands)))
       (map :n)
       (apply +)))

(defn get-power-of [color coll]
  (apply max (map #(get % color 1) coll)))
(defn get-power [coll]
  (apply * ((juxt (partial get-power-of :red)
                  (partial get-power-of :green)
                  (partial get-power-of :blue))
            coll)))

(defn part-2 [input]
  (->> (lines input)
       (map line->game)
       (map :hands)
       (map get-power)
       (apply +)))

#_(let [example "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]
    (line->game (second (lines example)))
    (part-1 example)
    (part-2 example))


#_(part-1 (get-input 2023 2))
#_(part-2 (get-input 2023 2))

(get-input 2023 5)
