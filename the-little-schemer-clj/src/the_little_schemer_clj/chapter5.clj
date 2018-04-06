(ns the-little-schemer-clj.chapter5)

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
