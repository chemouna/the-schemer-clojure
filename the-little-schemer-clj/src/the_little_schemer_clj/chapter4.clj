(ns the-little-schemer-clj.chapter4)

(defn add1
  [n]
  (inc n))

(defn sub1
  [n]
  (dec n))

(defn plus
  [n m]
  (cond
    (zero? m) n
    :else (add1 (plus n (sub1 m)))))

;(plus 2 3)

(defn minus
  [n m]
  (cond
    (zero? m) n
    :else (sub1 (minus n (sub1 m)))))

;(minus 5 2)
;(minus 0 1)
;(minus 1 0)

(defn addtup
  [tup]
  (cond
    (empty? tup) 0
    :else (plus (first tup) (addtup (rest tup)))))

(defn mult
  [n m]
  (cond
    (zero? m) 0
    :else (plus n (mult n (sub1 m)))))

;(mult 3 2)

(defn tup+
  [tup1 tup2]
  (cond
    (empty? tup1) tup2
    (empty? tup2) tup1
    :else (cons (plus (first tup1) (first tup2)) (tup+ (rest tup1) (rest tup2)))))

;(tup+ '(3 6 9 11 4) '(8 5 2 0 7))

(defn biggerThan
  [n m]
  (cond
    (zero? n) false
    (zero? m) true
    :else (biggerThan (sub1 n) (sub1 m))))

;(biggerThan 120 11)
;(biggerThan 12 133)
;(biggerThan 2 2)

(defn lessThan
  [n m]
  (cond
    (zero? m) false
    (zero? n) true
    :else (lessThan (sub1 n) (sub1 m))))

;(lessThan 11 120)
;(lessThan 133 12)
(biggerThan 2 2)

(defn equal
  [n m]
  (cond
    (or (biggerThan n m) (lessThan n m)) false
    :else true))

;(equal 1 2)
;(equal 3 3)

(defn exp
  [n m]
  (cond
    (zero? m) 1
    :else (mult n (exp n (sub1 m)))))

;(exp 2 3)
;(exp 1 1)
;(exp 5 3)

(defn pick
  [n coll]
  (cond
    (zero? (sub1 n)) (first coll)
    :else (pick (sub1 n) (rest coll))))

;(pick 2 '(1 2 3 4))
;(pick 0 '("a"))

(defn rempick
  [n coll]
  (cond
    (zero? (sub1 n)) (rest coll)
    :else (cons (first coll) (rempick (sub1 n) (rest coll)))))

;(rempick 2 '(1 2 3 4))

(defn no-nums
  [coll]
  (cond
    (empty? coll) ()
    (number? (first coll)) (no-nums (rest coll))
    :else (cons (first coll) (no-nums (rest coll)))))

;(no-nums '(5 2 "c"))
;(no-nums '(1 "a" 4 2 "c"))
