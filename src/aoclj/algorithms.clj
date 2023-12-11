(ns aoclj.algorithms)

(defn gcd
  "Greatest common divisor"
  ([a b]  (if (zero? b) a
              (recur b (mod a b))))
  ([a b & more] (reduce gcd a (into more [a b]))))

(defn lcm
  "Least common multiple"
  ([a b] (/ (* a b) (gcd a b)))
  ([a b & more] (reduce lcm 1 (into more [a b]))))

(defn quadratic-roots
  "Find roots of a hyperbola"
  [a b c]
  (let [D (Math/sqrt (- (* b b) (* 4 a c)))
        B (- b)
        A (* 2 a)]
    (cond
      (zero? D) [(/ B A)]
      (pos? D)  [(/ (+ B D) A) (/ (- B D) A)]
      :else     nil)))

(defn manhattan
  "Calculate manhattan distance between two points [x y]"
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2 ))))

(comment
  (gcd 1071 462 7)
  (lcm 140 72 9)
  ,)
