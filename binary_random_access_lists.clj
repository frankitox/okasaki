(ns binary-random-access-lists
  (:require
    [clojure.core.match :refer [match]]))


(defn third [coll]
  (nth coll 2))

(defn leaf [x]
  [x])

(defn link [t1 t2]
  (if (= 1 (count t1))
    [t1 t2 2]
    [t1 t2 (+ (third t1) (third t2))]))

(defn one [t]
  ['One t])

(defn zero []
  ['Zero])

(defn node-size [t]
  (nth t 2 1))

(defn ins-tree [t ts]
  (match ts
    ([['One t'] & ts'] :seq) (cons (zero) (ins-tree (link t t') ts'))
    ([['Zero] & ts'] :seq) (cons (one t) ts')
    :else (list (one t))))

(defn c0ns [x ts] (ins-tree (leaf x) ts))

(defn div-tree [t ts n]
  (println "DIV_TREE" n t)
  (if (> n 10)
    ts
    (do (println t)
      (match t
        [x] (cons (one [x]) ts)
        [l r m] (div-tree l (cons (one r) ts) (inc n))))))

(defn tail* [ts n]
  (println "TAIL" n)
  (if (> n 10)
    :StackOverflow
    (match ts
      ([['One t] & ts'] :seq) (div-tree t ts' 0)
      ([['Zero] & ts'] :seq) (tail* ts' (inc n))
      ([['One t]] :seq) (list))))

(defn borrow-tree [ts]
  (match ts
    ([['One t]] :seq) [t []]
    ([['One t] & ts'] :seq) [t (cons (zero) ts')]
    ([['Zero] & ts'] :seq) (let [[t ts''] (borrow-tree ts')]
                             (match t
                               [x] [[x] ts'']
                               [l r _] [l (cons (one r) ts'')]))))

(defn tail [ts]
  (second (borrow-tree ts)))

(defn head [ts]
  (ffirst (borrow-tree ts)))

(defn lookup-tree [t i]
  (match [t i]
    [[x] 0] x
    [[l r n] _] (let [half-n (/ n 2)]
                  (if (< i half-n)
                    (lookup-tree l i)
                    (lookup-tree r (- i half-n))))))

(defn lookup [ts i]
  (match ts
    ([] :seq) :IndexOutOfBounds
    ([['Zero] & ts'] :seq) (lookup ts' i)
    ([['One t] & ts'] :seq) (let [size (node-size t)]
                              (if (< i size)
                                (lookup-tree t i)
                                (lookup ts' (- i size))))))

(defn update-tree [t i y]
  (match [t i]
    [[x] 0] [y]
    [[l r n] _] (let [half-n (/ n 2)]
                  (if (< i half-n)
                    [(update-tree l i y) r n]
                    [l (update-tree r (- i half-n) y) n]))))

(defn updat3 [ts i x]
  (match ts
    ([] :seq) :IndexOutOfBounds
    ([['Zero] & ts'] :seq) (cons (zero) (updat3 ts' i x))
    ([['One t] & ts'] :seq) (let [size (node-size t)]
                              (if (< i size)
                                (cons (one (update-tree t i x)) ts')
                                (cons (one t) (updat3 ts' (- i size) x))))))

#_(tail (c0ns :OtherItem (c0ns :AnItem (list))))
#_(head (c0ns :OtherItem (c0ns :AnItem (list))))
#_(tail (c0ns :A (c0ns :More (c0ns :OtherItem (c0ns :AnItem (list))))))
#_(head (c0ns :A (c0ns :More (c0ns :OtherItem (c0ns :AnItem (list))))))
#_(lookup (c0ns :OtherItem (c0ns :AnItem (list))) 2)
#_(lookup (c0ns :A (c0ns :More (c0ns :OtherItem (c0ns :AnItem (list))))) 2)
#_(lookup (c0ns :A (c0ns :More (c0ns :OtherItem (c0ns :AnItem (list))))) 20)
#_(updat3 (c0ns :OtherItem (c0ns :AnItem (list))) 1 :OOO)
#_(updat3 (c0ns :A (c0ns :More (c0ns :OtherItem (c0ns :AnItem (list))))) 2 :COCO)
#_(updat3 (c0ns :A (c0ns :More (c0ns :OtherItem (c0ns :AnItem (list))))) 20 :COCO)

;; cons, head, and tail perform at most O(1) work per digit and so run in O(log n) worstcase
;; time. lookup and update take at most O(log n) time to find the right tree, and then at most
;; O(log n) time to find the right element in that tree, for a total of O(log n) worst-case time.
