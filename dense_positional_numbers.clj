(ns dense-positional-numbers
  (:require
    [clojure.core.match :refer [match]]))


(defn inc [n]
  (match n
    (['One & xs] :seq) (cons 'Zero (inc xs))
    (['Zero & xs] :seq) (cons 'One xs)
    :else '(One)))

(defn dec [n]
  (match n
    (['One] :seq) '()
    (['One & xs] :seq) (cons 'Zero xs)
    (['Zero & xs] :seq) (cons 'One (dec xs))))

(defn add [xs ys]
  (match [xs ys]
    [([] :seq) _] ys
    [_ ([] :seq)] xs
    [(['Zero & xs'] :seq) ([y & ys'] :seq)] (cons y (add xs' ys'))
    [([x & xs'] :seq) (['Zero & ys'] :seq)] (cons x (add xs' ys'))
    [(['One & xs'] :seq) (['One & ys'] :seq)] (cons 'Zero (inc (add xs' ys')))))
