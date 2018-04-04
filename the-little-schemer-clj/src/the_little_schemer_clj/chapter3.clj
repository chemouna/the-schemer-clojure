(ns the-little-schemer.chapter3
  (:use the-little-schemer.chapter2))

(defn rember
  [a lat]
  (cond
    (empty? lat) ()
    (= a (first lat)) (rest lat)
    :else (conj (rember a (rest lat))
                (first lat))))

(defn firsts
  [coll]
    (cond
      (empty? coll) ()
      :else (cons (first (first coll))
                  (firsts (rest coll)))))

;(firsts '((1 2 3) (4 5 6) (8 9)))

(defn insertR
  [new old lat]
  (cond
    (empty? lat) lat
    (= old (first lat)) (cons old (cons new (rest lat)))
    :else (cons (first lat) (insertR new old (rest lat)) )))

;(insertR 4 3 '(1 2 3 5 6))
;(insertR 2 1 '())

(defn insertL
  [new old lat]
  (cond
    (empty? lat) lat
    (= old (first lat)) (cons new (cons old (rest lat))) ;; can also just use (cons new lat)
    :else (cons (first lat) (insertL new old (rest lat)))))

;(insertL 2 3 '(1 3 5 6))
;(insertL 2 1 '())

(defn subst
  [new old lat]
  (cond
    (empty? lat) lat
    (= old (first lat)) (cons new (rest lat))
    :else (cons (first lat) (subst new old (rest lat)))))

;(subst 1 2 '(1 2 4))

(defn subst2
  [new o1 o2 lat]
  (cond
    (empty? lat) lat
    (or (= o1 (first lat)) (= o2 (first lat))) (cons new (rest lat))
    :else (cons (first lat) (subst2 new o1 o2 (rest lat)))))

;(subst2 "vanilla" "chocolate" "banana" '("banana" "ice" "cream" "with" "chocolate" "topping"))

