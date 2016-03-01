(ns the-little-schemer)

;; Lambda the utlimate

(def rember-f?
  (fn [test? a l]
    (cond
      (nil? l) '()
      true (cond
             (test? (first l) a) (rest l)
             true (cons (first l) (rember-f? test? a (rest l)))))))


(println (rember-f? = '(pop corn) '(lemonade (pop corn) and (cake))))
