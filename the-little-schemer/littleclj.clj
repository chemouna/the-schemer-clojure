(ns the-little-schemer)

;; Lambda the utlimate

;; 1st version
(def rember-f?
  (fn [test? a l]
    (cond
      (nil? l) '()
      true (cond
             (test? (first l) a) (rest l)
             true (cons (first l) (rember-f? test? a (rest l)))))))


(println (rember-f? = '(pop corn) '(lemonade (pop corn) and (cake))))

;; 2nd version: removing the second condition
(def rember-f2?
  (fn [test? a l]
    (cond
      (nil? l) '()
      (test? (first l) a) (rest l)
      true (cons (first l) (rember-f? test? a (rest l))))))

(println (rember-f2? = '(pop corn) '(lemonade (pop corn) and (cake))))

;; 3d version
(def rember-f3?
  (fn [test? a l]
      (nil? l) '()
      (test? (first l) a) (rest l)
      (cons (first l) (rember-f? test? a (rest l)))))

(println (rember-f3? = '(pop corn) '(lemonade (pop corn) and (cake))))

(println (rember-f2? = '(a) ())) ;; SO error!

  ;; TODO: let's do rember-f in core.logic with pattern matching ? 
