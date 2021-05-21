(ns binomial-heaps
  (:require
    [clojure.core.match :refer [match]]))


(defn leaf [x]
  [0 x nil]) ;; rank value children

(defn rank [t]
  (first t))

(defn root [t]
  (second t))

(defn link [t1 t2]
  (match [t1 t2]
    [[r x ts1] [r' y ts2]] (if (< x y)
                             [(inc r) x (cons [r y ts2] ts1)]
                             [(inc r') y (cons [r x ts1] ts2)])))

(defn carry [t ts]
  (match [ts]
    [([] :seq)] (list t)
    [([t' & ts'] :seq)] (if (< (rank t) (rank t'))
                          (cons t ts)
                          (carry (link t t') ts'))))

(defn insert [x ts]
  (carry (leaf x) ts))

(defn merge* [ts1 ts2]
  (match [ts1 ts2]
    [_ ([] :seq)] ts1
    [([] :seq) _] ts2
    [([t1 & ts1'] :seq) ([t2 & ts2'] :seq)] (cond
                                              (< (rank t1) (rank t2)) (cons t1 (merge* ts1' ts2))
                                              (> (rank t1) (rank t2)) (cons t2 (merge* ts1 ts2'))
                                              (= (rank t1) (rank t2)) (carry (link t1 t2) (merge* ts1' ts2')))))

(defn find-min [ts]
  (match [ts]
    [([] :seq)] :EmptyTree
    [([t] :seq)] (root t)
    [([t & ts'] :seq)] (min (root t) (find-min ts'))))

(defn min-tree [ts]
  (if (= 1 (count ts))
    [(first ts) (list)]
    (match [ts]
      [([t & ts'] :seq)] (let [[t' ts''] (min-tree ts')]
                           (if (< (root t) (root t'))
                             [t ts']
                             [t' (cons t ts'')])))))

(defn delete-min [ts]
  (let [[[_ _ ts1] ts2] (min-tree ts)]
    (merge* ts1 ts2)))

#_(link (leaf 3) (link (leaf 20) (leaf 1)))
#_(insert 4 (insert 30 (insert 2 (insert 10 '()))))
#_(merge* (insert 4 (insert 30 (insert 2 (insert 10 '())))) (insert 3 (insert 20 (insert 1 '()))))
#_(find-min (merge* (insert 4 (insert 30 (insert 2 (insert 10 '())))) (insert 3 (insert 20 (insert 1 '())))))
#_(delete-min (merge* (insert 4 (insert 30 (insert 2 (insert 10 '())))) (insert 3 (insert 20 (insert 1 '())))))

;; insert, merge heaps, findMin, deleteMin
