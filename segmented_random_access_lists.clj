(ns segmented-random-access-lists
  (:require
    [clojure.core.match :refer [match]]))

;; 00111 -> 1101
;; DEQ[3,5] -> DEQ[1,2] + DEQ[4]

;; [rank value children]

(defn leaf [x]
  [1 x '()]) ;; # children, value, children

;; 101 -> 011
;; [[:Ones m] [:Zeros 1] [:Ones n]] -> [[:Zeros m] [:Ones (inc n)]]
;; 1001 -> 0101
;; [[:Ones m] [:Zeros n] [:Ones 1]] -> [[:Zeros m] [:Ones 1] [:Zeros (dec n)] [:Ones 1]]
;; 01 -> 11
;; [[:Zero 1] [:Ones n]] -> [[:Ones (inc n)]]
;; 00 -> 10
;; [[:Zero m]] -> [[:Ones 1] [:Zeros (dec m)]]

(defn zeros [n ts]
  (match [n ts]
    [_ ([] :seq)] (list)
    [_ ([[:Zeros m] & ts'] :seq)] (cons [:Zeros (+ n m)] ts')
    [0 _] ts
    [_ _] (cons [:Zeros n] ts)))

(defn ones [n child ts]
  (match [n ts]
    [_ ([[:Ones m children] & ts'] :seq)] (cons [:Ones (+ n m) (cons child children)] ts')
    [0 _] ts
    [_ _] (cons [:Ones n (list child)] ts)))

(defn c0ns- [c ts]
  (match [ts]
    [([] :seq)] (cons [:Ones 1 (list c)] (list))
    [([[:Zeros n] & ts'] :seq)] (ones 1 c (zeros (dec n) ts'))
    [([[:Ones n children] & ts'] :seq)] (cons [:Zeros n] (c0ns- [(inc n) (second c) children] ts'))))

(defn c0ns [x ts]
  (c0ns- (leaf x) ts))

(defn ones* [n children ts]
  (match [n ts]
    [0 _] ts
    [_ _] (cons [:Ones n children] ts)))

(defn dec-aux [ts]
  (match [ts]
    [([[:Ones n children] & ts'] :seq)] [(first children) (zeros 1 (ones* (dec n) (rest children) ts'))]
    [([[:Zeros n] & ts'] :seq)] (let [[child ts''] (dec-aux ts')]
                                  [(leaf (nth child 1)) (ones* n (nth child 2) ts'')])))

(defn dec* [ts]
  (second (dec-aux ts)))

(def a-h (reduce #(c0ns %2 %1) '() [:a :b :c :d :e :f :g :h]))
#_(first (nth (iterate dec* a-h) 7))
#_(dec* (dec* (reduce #(c0ns %2 %1) '() [:a :b :c :d :e :f :g :h])))

(defn head [ts]
  (second (first (dec-aux ts))))

(defn log2 [x]
  (int (Math/floor (/ (Math/log x) (Math/log 2)))))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn lookup-tree [[r v children] i]
  (if (< i 1)
    v
    (let [idx (log2 i)]
      (lookup-tree (nth children idx) (- i (exp 2 idx))))))

(defn lookup* [ts i d]
  (println "Hello" i)
  (match [ts]
    [([[:Ones n children] & ts'] :seq)] (let [to (exp 2 (+ d n))]
                                          (if (< i to)
                                            (let [idx (log2 (inc i))
                                                  offset (exp 2 idx)]
                                              (println to idx offset (count children))
                                              (println "GOOD")
                                              (lookup-tree (nth children idx) (- i offset)))
                                            (lookup* ts' i (+ d n))))
    [([[:Zeros n] & ts'] :seq)] (lookup* ts' i d)))

(defn lookup [ts i]
  (lookup* ts i 0))

#_(lookup a-h 1)

;; 111 -> 0001
;; [[:Ones 3]] -> [[:Zeros 3] [:Ones 1]]
;; 001 -> 101
;; [[:Zeros 2] [:Ones 1]] -> [[:Ones 1] [:Zeros 1] [:Ones 1]]
;; 011 -> 111
;; [[:Zeros 1] [:Ones 2]] -> [[:Ones 3]]
