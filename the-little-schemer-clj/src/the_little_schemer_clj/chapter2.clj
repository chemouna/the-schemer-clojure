(ns the-little-schemer-clj.chapter2)

(def atom?
  (complement coll?))

(def lat?
  (fn [l]
    (cond
      (empty? l) true
      (atom? (first l)) (lat? (rest l))
      :else false)))

(defn member?
  [a lat]
  (cond
    (empty? lat) false
    :else (or (= (first lat) a)
              (recur a (rest lat)))))

