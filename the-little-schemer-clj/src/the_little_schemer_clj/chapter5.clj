(ns the-little-schemer-clj.chapter5)

(defn rember*
  [a lat]
  (cond
    (empty? lat) lat
    (= a (first lat)) (rember* a (rest lat))
    :else
    (cond
      (list? (first lat)) (cons (rember* a (first lat)) (rember* a (rest lat)))
      :else (cons (first lat) (rember* a (rest lat))))))

(defn rember2*
  [a lat]
    (cond
      (empty? lat) ()
      (not (list? (first lat)))
            (cond
              (= (first lat) a) (rember* a (rest lat))
              :else (cons (first lat) (rember* a (rest lat))))
      :else (cons (rember* a (first lat)) (rember* a (rest lat)))))

;(rember* 1 '(1 (2 1) 3))
;(rember* "cup" '(("coffee") "cup" (("tea") "cup") ("and" ("hick")) "cup"))

(rember2* 1 '(1 (2 1) 3))
(rember2* "cup" '(("coffee") "cup" (("tea") "cup") ("and" ("hick")) "cup"))
