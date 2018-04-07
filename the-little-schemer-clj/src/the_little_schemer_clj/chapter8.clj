(ns the-little-schemer-clj.chapter8
  (:use [the-little-schemer-clj.chapter2]
        [the-little-schemer-clj.chapter3]
        [the-little-schemer-clj.chapter4]
        [the-little-schemer-clj.chapter6]))

(defn rember-f
  [test? a l]
  (cond
    (empty? l) l
    (test? (first l) a) (rest l)
    :else (cons (first l) (rember-f test? a (rest l)))))

(defn rember-f2
  [test?]
  (fn [a lat]
    (cond
      (empty? lat) ()
      (test? a (first lat)) (rest lat)
      :else (cons (first lat) ((rember-f2 test?) a (rest lat))))))

(def rember-eq? (rember-f2 =))

(defn insertL-f
  [test?]
  (fn [new old lat]
    (cond
      (empty? lat) lat
      (test? (first lat) old) (cons new (cons old (rest lat)))
      :else (cons (first lat) ((insertL-f test?) new old (rest lat))))))

(def insertL-eq (insertL-f =))

(defn insertR-f
  [test?]
  (fn [new old lat]
    (cond
      (empty? lat) lat
      (test (first lat) old) (cons old (cons new (rest lat)))
      :else (cons (first lat) ((insertR-f test?) new old (rest lat)) ))))

(defn seqL
  [new old coll]
  (cons new (cons old coll)))

(defn seqR
  [new old coll]
  (cons old (cons new coll)))

(defn insert-g
  [seq]
  (fn [new old lat]
    (cond
      (empty? lat) lat
      (= (first lat) old) (seq new old (rest lat))
      :else (cons (first lat) ((insert-g seq) new old (rest lat))))))

(def insertL2 (insert-g seqL))
(def insertR2 (insert-g seqR))

(defn seqSubst
  [new old lat]
  (cons new (rest lat)))

(def subst3 (insert-g seqSubst))

(defn atom-to-function
  [x]
  (cond
    (= x '+) plus
    (= x '-) minus
    (= x '*) mult
    (= x 'exp) exp))

(defn value3
  [nexp]
  (cond
    (atom? nexp) nexp
    :else ((atom-to-function (operator nexp))
           (value3 (first-sub-exp nexp))
           (value3 (second-sub-exp nexp)))))

(defn multirember-f
  [test?]
  (fn [a lat]
    (cond
      (empty? lat) lat
      (test? (first lat) a) ((multirember-f test?) a (rest lat))
      :else (cons (first lat) ((multirember-f test?) a (rest lat))))))

(def multirember-eq (multirember-f =))

;(multirember-eq 2 '(1 2 3 4 5 2 8 2 2))
