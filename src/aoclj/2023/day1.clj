(ns aoclj.2023.day1
  (:require [aoclj.tools :refer [get-input lines]]
            [clojure.string :as str]))

(defn to-digit [s]
  (case s
    "one" "1"
    "two" "2"
    "three" "3"
    "four" "4"
    "five" "5"
    "six" "6"
    "seven" "7"
    "eight" "8"
    "nine" "9"
    s))

(defn extract-number [regex line]
  (->> (re-seq regex line)
       flatten
       (filter (complement empty?))
       ((juxt first last))
       (map to-digit)
       str/join
       parse-long))

(defn solve [regex input]
  (->> (lines input)
       (map (partial extract-number regex))
       (apply +)))

(def part-1 (partial solve #"\d"))

(def part-2 (partial solve #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))"))

#_(let [example "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"]
    (part-1 example))

#_(extract-number #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))" "twoneighthree")

#_(let [example "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"]
    (part-2 example))

#_(part-1 (get-input 2023 1))
#_(part-2 (get-input 2023 1))
