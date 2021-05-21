(ns sparse-positional-numbers
  (:require
    [clojure.core.match :refer [match]]))

(defn carry [w ws]
  (match [ws]
    [([] :seq)] (list w)
    [([w' & ws'] :seq)] (if (< w w')
                          (cons w ws)
                          (carry (* w 2) ws'))))

(defn borrow [w ws]
  (match [ws]
    [([w' & ws'] :seq)] (if (= w w')
                          ws'
                          (cons w (borrow (* w 2) ws)))))

(defn inc [n]
  (carry 1 n))

(defn dec [n]
  (borrow 1 n))

(defn add [n m]
  (match [n m]
    [_ ([] :seq)] n
    [([] :seq) _] m
    [([x & xs] :seq) ([y & ys] :seq)] (cond
                                        (< x y) (cons x (add xs m))
                                        (> x y) (cons y (add n ys))
                                        (= x y) (carry (* x 2) (add xs ys)))))

#_(borrow 1 (borrow 1 (borrow 1 (inc (inc (inc (inc '())))))))
#_(add (inc (inc (inc (inc '())))) (inc (inc (inc (inc '())))))
