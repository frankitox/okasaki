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

#_(borrow 1 (borrow 1 (borrow 1 (inc (inc (inc (inc '())))))))
