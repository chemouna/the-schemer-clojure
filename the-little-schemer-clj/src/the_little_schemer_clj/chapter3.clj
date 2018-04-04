(ns the-little-schemer-clj.chapter3)

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

