(ns the-little-schemer-clj.chapter8
  (:use [the-little-schemer-clj.chapter2]
        [the-little-schemer-clj.chapter3]
        [the-little-schemer-clj.chapter4]
        [the-little-schemer-clj.chapter6]
        [clojure.tools.trace :as trace]))

(trace/trace-ns 'the-little-schemer-clj.chapter8)

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

(defn multiremberT
  [test? lat]
  (cond
    (empty? lat) lat
    (test? (first lat)) (multiremberT test? (rest lat))
    :else (cons (first lat) (multiremberT test? (rest lat)))))

(defn eq?-tuna
  [x]
  (= x 'tuna))

(defn multiinsertLR
  [new oldl oldr lat]
  (cond
    (empty? lat) lat
    (= (first lat) oldl) (cons new (cons oldl (rest lat)))
    (= (first lat) oldr) (cons oldr (cons new (rest lat)))
    :else (cons (first lat) (multiinsertLR new oldl oldr (rest lat)))))

(defn multirember&co
  [a lat col]
  (cond
    (empty? lat) (col '() '())
    (= (first lat) a) (multirember&co a (rest lat)
                                      (fn [newlat seen]
                                        (col newlat (cons (first lat) seen))))
    :else (multirember&co a (rest lat)
                          (fn [newlat seen]
                            (col (cons (first lat) newlat) seen)))))

;; it seems to be a function that adds the values equal to a into the collection seen and delete them from the passed;; list and those that dont match into new lat

(defn a-friend
  [x y]
  (empty? y))

;(multirember&co "tuna" '("strawberries" "tuna" "and" "swordfish") a-friend)
;(multirember&co "tuna" '() a-friend)
;(multirember&co "tuna" '("tuna") a-friend)

(defn multiinsertLR&co
  [new oldl oldr lat col]
  (cond
    (empty? lat) (col '() 0 0)
    (= (first lat) oldl) (multiinsertLR&co new oldl oldr (rest lat)
                                          (fn [newlat cl cr]
                                            (col (cons new (cons oldl newlat)) (add1 cl) cr)))
    (= (first lat) oldr) (multiinsertLR&co new oldl oldr (rest lat)
                                           (fn [newlat cl cr]
                                             (col (cons oldr (cons new newlat)) cl (add1 cr))))
    :else (multiinsertLR&co new oldl oldr (rest lat)
                            (fn [newlat cl cr]
                              (col (cons (first lat) newlat) cl cr)))))

(defn evens-only*
  [l]
  (cond
    (empty? l) l
    (atom? (first l)) (cond
                        (even? (first l)) (cons (first l) (evens-only* (rest l)))
                        :else (evens-only* (rest l)))
    :else (cons (evens-only* (first l)) (evens-only* (rest l)))))

; (evens-only* '((9 1 28) 3 10 ((9 9) 76) 2))

(defn evens-only*&co
  [l col]
  (cond
    (empty? l) (col '() 1 0)
    (atom? (first l)) (cond
                        (even? (first l)) (evens-only*&co (rest l)
                                                          (fn [newlat me so]
                                                            (col (cons (first l) newlat) (* me (first l)) so)))
                        :else (evens-only*&co (rest l)
                                        (fn [newlat me so]
                                          (col newlat me (+ so (first l))))))
    :else (evens-only*&co (first l)
                          (fn [fnl fme fso]
                            (evens-only*&co (rest l)
                                            (fn [newlat me so]
                                              (col (cons fnl newlat) (* fme me) (+ fso so))))))))

(defn the-last-friend
  [newl product sum]
  (cons sum (cons product newl)))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)

