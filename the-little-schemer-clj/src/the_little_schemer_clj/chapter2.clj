(ns the-little-schemer.chapter2)

(def lat?
  (fn [l]
    (cond
      (empty? l) true
      (atom? (first l)) (lat? (rest l))
      :else false)))
