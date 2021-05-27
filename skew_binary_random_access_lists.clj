(ns skew-binary-random-access-lists
  (:require
    [clojure.core.match :refer [match]]))


(defn nth-iter [f x i]
  (nth (iterate f x) i))

(defn div2 [x]
  (bit-shift-right x 1))

(defn one [x]
  [1 [x]])

(defn cons' [x ts]
  (match [ts]
    [([[r1 t1] [r2 t2] & ts'] :seq)] (if (= r1 r2) (cons [(+ 1 r1 r2) [x t1 t2]] ts') (cons (one x) ts))
    :else (cons (one x) ts)))

(defn helper [ts]
  (match [ts]
    [([[1 t] & ts'] :seq)] [(first t) ts']
    [([[r [v lt rt]] & ts'] :seq)] [v (cons [(div2 (dec r)) lt] (cons [(div2 (dec r)) rt] ts'))]))

(defn tail [ts]
  (second (helper ts)))

(defn head [ts]
  (first (helper ts)))

(defn lookup-tree [t i size]
  (match [i t]
    [0 [v]] v
    [_ [_]] :IndexOutOfBounds
    [0 [v _ _]] v
    [_ [_ lt rt]] (let [prev-size (div2 (dec size))]
                    (if (< (dec i) prev-size)
                      (lookup-tree lt (dec i) prev-size)
                      (lookup-tree rt (- i 1 prev-size) prev-size)))))

(defn lookup [ts i]
  (match [ts]
    [([] :seq)] :IndexOutOfBounds
    [([[n t] & ts'] :seq)] (if (< i n)
                             (lookup-tree t i n)
                             (lookup ts' (- i n)))))

(defn update-tree [t i size x]
  (match [i t]
    [0 [_]] [x]
    [_ [_]] :IndexOutOfBounds
    [0 [v lt rt]] [x lt rt]
    [_ [v lt rt]] (let [prev-size (div2 (dec size))]
                    (if (< (dec i) prev-size)
                      [v (update-tree lt (dec i) prev-size x) rt]
                      [v lt (update-tree rt (- i 1 prev-size) prev-size x)]))))

(defn update' [ts i x]
  (match [ts]
    [([] :seq)] :IndexOutOfBounds
    [([[n t] & ts'] :seq)] (if (< i n)
                             (update-tree t i n x)
                             (update' ts' (- i n)))))

#_(reduce #(cons' %2 %1) '() [:a :b :c :d :e :f :g])
#_(nth-iter tail (reduce #(cons' %2 %1) '() [:a :b :c :d :e :f :g]) 6)
#_(lookup (reduce #(cons' %2 %1) '() [:a :b :c :d :e :f :g]) 6)
#_(update' (reduce #(cons' %2 %1) '() [:a :b :c :d :e :f :g]) 6 :coco)
