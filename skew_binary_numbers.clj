(ns skew-binary-numbers
  (:require
    [clojure.core.match :refer [match]]))


(defn exp [x n]
  (reduce * (repeat n x)))

(defn nth-iter [f x i]
  (nth (iterate f x) i))

(defn ->decimal [xs]
  (loop [[x & xs'] xs
         i 0
         acc 0]
    (if x
      (recur xs' (inc i) (+ acc (* x (dec (exp 2 (inc i))))))
      acc)))

#_(->decimal [0 0 2 1 0 1]) ;; 92

(defn inc' [xs]
  (match [xs]
    [([x y & xs'] :seq)] (if (= x y) (cons (+ 1 x y) xs') (cons 1 xs))
    :else (cons 1 xs)))

(defn dec' [xs]
  (match [xs]
    [([1 & xs'] :seq)] xs'
    [([x & xs'] :seq)] (let [half (/ (dec x) 2)]
                         (cons half (cons half xs')))))
(nth-iter inc' '() 4)

#_(= (nth-iter inc' '() 90) (nth-iter dec' (nth-iter inc' '() 100) 10))
