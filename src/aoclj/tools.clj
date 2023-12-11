(ns aoclj.tools
  (:require [hato.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def session-cookie (atom "53616c7465645f5fc3515d656e14615d0cd706a1dbb367cf1faffa727bf76634cd783f644021010d6edab366c788e086d217a1dd80b97a7d7c778bddf845dd64"))

(defn set-session-cookie! [value]
  (reset! session-cookie value))

(defn get-or-fetch [base-path file fetcher]
  (let [path (format "%s/%s" base-path file)]
    (when-not (.exists (io/file base-path))
      (prn "create dir" base-path)
      (.mkdirs (io/file base-path)))
    (when-not (.exists (io/file path))
      (prn "fetch input")
      (spit path (:body (fetcher))))
    (prn "getting from cache" path)
    (slurp path)))

(defn fetch
  [year day cookie]
  (let [url (format "https://adventofcode.com/%d/day/%d/input" year day)]
    (http/get url {:headers {"cookie" (format "session=%s" cookie)}})))

(defn get-input [year day]
  (let [fetcher (fn [](fetch year day @session-cookie))]
    (get-or-fetch (str "resources/" year) (str "day-" day) fetcher)))

#_(get-input 2023 1)
#_(fetch 2023 1 "53616c7465645f5fc3515d656e14615d0cd706a1dbb367cf1faffa727bf76634cd783f644021010d6edab366c788e086d217a1dd80b97a7d7c778bddf845dd64")

;; Text processing

(defn lines [s] (str/split s #"\n"))
(defn words [s] (str/split s #"\W"))

;; Map processing

(defn map-values [f coll]
  (into {} (for [[k v] coll] [k (f v)])))

(defn map-keys [f coll]
  (into {} (for [[k v] coll] [(f k) v])))

(defn scan
  ([f coll] (scan f nil coll))
  ([f initial coll]
   (reverse (reduce
             (fn [prev-coll curr] (cons (f (first prev-coll) curr) prev-coll))
             initial
             coll))))

(defn contains-all [[a & as] bs]
  (cond
    (nil? a) true
    (empty? bs) false
    (some #{a} bs) (recur as (assoc (vec bs) (.indexOf (vec bs) a) nil))
    :else false))

(comment
  (#'contains-all [1 2] [1 2 3])
  (#'contains-all [3] [1 2 3])
  (#'contains-all [5] [1 2 3])
  ,)

;; Arrays...

(defn find-with-coords [pred to-data rows]
  (let [w (count (first rows))
        h (count rows)
        rows (mapv vec rows)]
    (for [x (range w)
          y (range h)
          :let [cell (get-in rows [y x])]
          :when (pred cell)]
      [[x y] (to-data cell)])))

(def map-with-coords (partial find-with-coords (constantly true)))

(defn columns [rows]
  (apply mapv vector rows))

(comment
  (#'columns [[1 2 3] [4 5 6] [7 8 9]])
  ,)

(defn filter-indexed [pred coll]
  (let [w (count coll)]
    (for [x (range w)
          :let [col (nth coll x)]
          :when (pred col)]
      [x col])))

(defn filter-columns [pred rows] (filter-indexed pred (columns rows)))
(def filter-rows filter-indexed)

