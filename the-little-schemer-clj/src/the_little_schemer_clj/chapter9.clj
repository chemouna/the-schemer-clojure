
(ns the-little-schemer-clj.chapter9
  (:use [the-little-schemer-clj.chapter2]
        [the-little-schemer-clj.chapter3]
        [the-little-schemer-clj.chapter4]
        [the-little-schemer-clj.chapter7]
        [clojure.tools.trace :as trace]))

(trace/trace-ns 'the-little-schemer-clj.chapter9)

(defn keep-looking
  [a sorn lat]
  (cond
    (number? sorn) (keep-looking a (pick sorn lat) lat)
    :else (= sorn a)))

(defn looking
  [a lat]
    (keep-looking a (pick 1 lat) lat))

(def lat '(6 2 4 "caviar" 5 7 3))
(looking "caviar" lat)
(pick 6 lat)
(keep-looking "caviar" 7 lat)

; (looking "caviar" '(7 2 4 7 5 6 3)) ; StackOverflowError
; (looking "caviar" '(7 1 2 "caviar" 5 6 3)) ; StackOverflowErro

(defn shift
  [x]
  (build (first (first x))
         (build (second (first x)) (rest x))))

(shift '(("a" "b") "c"))
