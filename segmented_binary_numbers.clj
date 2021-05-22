(ns segmented-binary-numbers
  (:require
    [clojure.core.match :refer [match]]))


(defn seg-size [[_ n]] n)

(defn ones [n xs]
  (match [xs]
    [([] :seq)] (cons [:Ones n] (list))
    [([[:Ones m] & xs'] :seq)] (cons [:Ones (+ n m)] xs')
    :else (if (zero? n) xs (cons [:Ones n] xs))))

(defn zeros [n xs]
  (match [xs]
    [([] :seq)] (list)
    [([[:Zeros m] & xs'] :seq)] (cons [:Zeros (+ n m)] xs')
    :else (if (zero? n) xs (cons [:Zeros n] xs))))

(defn inc* [xs]
  (match [xs]
    [([] :seq)] (ones 1 (list))
    [([[:Zeros m] & xs'] :seq)] (ones 1 (zeros (dec m) xs'))
    [([[:Ones m] & xs'] :seq)] (zeros m (inc* xs'))))

(defn dec* [xs]
  (match [xs]
    [([] :seq)] (zeros 1 (list))
    [([[:Ones 1] & xs'] :seq)] (zeros 1 xs')
    [([[:Ones m] & xs'] :seq)] (zeros 1 (ones (dec m) xs'))
    [([[:Zeros m] & xs'] :seq)] (ones m (dec* xs'))))

#_(nth (iterate inc* (list)) 7)
#_(dec* (inc* (list)))
#_(dec* (nth (iterate inc* (list)) 8))
