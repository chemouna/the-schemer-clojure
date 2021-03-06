(ns the-little-schemer-clj.chapter3
  (:use the-little-schemer-clj.chapter2))

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

(defn multirember
  [a lat]
  (cond
    (empty? lat) lat
    :else (cond
            (= a (first lat)) (rember a (rest lat))
            :else (cons (first lat) (rember a (rest lat))))))

;(multirember "banana" '("banana" "ice" "cream" "with" "banana" "topping"))

(defn multiinsertR
  [new old lat]
  (cond
    (empty? lat) lat
    (= old (first lat)) (cons old (cons new (multiinsertR new old (rest lat))))
    :else (cons (first lat) (multiinsertR new old (rest lat)) )))

; (multiinsertR 1 2 '(2 3 4 5 2 8 9))

(defn multiinsertL
  [new old lat]
  (cond
    (empty? lat) lat
    (= old (first lat)) (cons new (cons old (multiinsertL new old (rest lat))))
    :else (cons (first lat) (multiinsertL new old (rest lat)) )))

; (multiinsertL 1 2 '(2 3 4 5 2 8 9))

(defn multisubst
  [new old lat]
  (cond
    (empty? lat) lat
    (= old (first lat)) (cons new (multisubst new old (rest lat)))
    :else (cons (first lat) (multisubst new old (rest lat)))))

;(multisubst 2 3 '(1 3 4 3 3 6))
