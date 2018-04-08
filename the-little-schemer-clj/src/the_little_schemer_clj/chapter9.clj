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

(defn align
  [pora]
  (cond
    (atom? pora) pora
    (a-pair? (first pora)) (align (shift pora))
    :else (build (first pora)
                 (align (second pora)))))

;(align '("a"))

(defn length*
  [pora]
  (cond
    (atom? pora) 1
    (empty? pora) 0
    :else (+ (length* (first pora)) (length* (rest pora)))))

;(length* '("a" ("b" "c") ("d" "g")))

(defn weight*
  [pora]
  (cond
    (atom? pora) 1
    :else (+ (* (weight* (first pora)) 2) (weight* (second pora)))))

;(weight* '(("a" "b") "c"))
;(weight* '("a" ("b" "c")))

(defn shuffle
  [pora]
  (cond
    (atom? pora) pora
    (a-pair? (first pora)) (shuffle (revpair pora))
    :else (build (first pora)
                 (shuffle (second pora)))))

;(shuffle '(("a" "b") "c"))
;(shuffle '("a" ("b" "c")))
;(shuffle '("a" "b"))

;(shuffle '(("a" "b") ("e" "d"))) ; this one cause a StackOverflowError => shuffle is not total

(defn A
  [n m]
  (cond
    (zero? n) (add1 m)
    (zero? m) (A (sub1 n) 1)
    :else (A (sub1 n) (A n (sub1 m)))))

;(A 1 2)
;(A 0 (A 1 1))
;(A 0 3)
;(A 4 3)
