(ns the-little-schemer-clj.chapter5
  (:use the-little-schemer-clj.chapter4))

(defn rember*
  [a l]
  (cond
    (empty? l) l
    (= a (first l)) (rember* a (rest l))
    :else
    (cond
      (list? (first l)) (cons (rember* a (first l)) (rember* a (rest l)))
      :else (cons (first l) (rember* a (rest l))))))

(defn rember2*
  [a l]
    (cond
      (empty? l) ()
      (not (list? (first l)))
            (cond
              (= (first l) a) (rember* a (rest l))
              :else (cons (first l) (rember* a (rest l))))
      :else (cons (rember* a (first l)) (rember* a (rest l)))))

;(rember* 1 '(1 (2 1) 3))
;(rember* "cup" '(("coffee") "cup" (("tea") "cup") ("and" ("hick")) "cup"))

;(rember2* 1 '(1 (2 1) 3))
;(rember2* "cup" '(("coffee") "cup" (("tea") "cup") ("and" ("hick")) "cup"))

(defn insertR*
  [new old l]
  (cond
    (empty? l) l
    (= old (first l)) (cons old (cons new (insertR* new old (rest l))))
    :else
    (cond
      (list? (first l)) (cons (insertR* new old (first l)) (insertR* new old (rest l)))
      :else (cons (first l) (insertR* new old (rest l))))))

;(insertR* 1 2 '(2 (3 2) (2 4)))

(defn occur*
  [a coll]
  (cond
    (empty? coll) 0
    (= a (first coll)) (add1 (occur* a (rest coll)))
    :else (cond
            (list? (first coll)) (plus (occur* a (first coll)) (occur* a (rest coll)))
            :else (occur* a (rest coll)))))

; (occur* 2 '(1 (3 2) (5 2 8) 2 4))

(defn subst*
  [new old coll]
  (cond
    (empty? coll) coll
    (= old (first coll)) (cons new (subst* new old (rest coll)))
    :else (cond
            (list? (first coll)) (cons (subst* new old (first coll)) (subst* new old (rest coll)))
            :else (cons (first coll) (subst* new old (rest coll))))))

; (subst* 3 1 '(2 1 6 1 1 1 8 1))

(defn insertL*
  [new old coll]
  (cond
    (empty? coll) coll
    (= old (first coll)) (cons new (cons old (insertL* new old (rest coll))))
    :else (cond
            (list? (first coll)) (cons (insertL* new old (first coll)) (insertL* new old (rest coll)))
            :else (cons (first coll) (insertL* new old (rest coll))))))

; (insertL* 3 1 '(2 1 6 8 1 9))

(defn member*
  [a coll]
  (cond
    (empty? coll) false
    :else (or (= (first coll) a)
              (and (list? (first coll)) (member* a (first coll)))
              (member* a (rest coll)))))

(defn member2*
  [a coll]
  (cond
    (empty? coll) false
    (not (list? (first coll))) (or (= (first coll) a) (member2* a (rest coll)))
    :else (or (member2* a (first coll)) (member2* a (rest coll)))))

;(member* 7 '(1 (2 3) 5 (7 4) 2 (6 9)))
;(member2* 7 '(1 (2 3) 5 (7 4) 2 (6 9)))

(defn leftmost
  [coll]
  (cond
    (not (list? (first coll))) (first coll)
    :else (leftmost (first coll))))

;(leftmost '((1 2) 3))
;(leftmost '(3 4))
;(leftmost '((6 2 1)))
