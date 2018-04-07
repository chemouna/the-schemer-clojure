(ns the-little-schemer-clj.chapter6
  (:use [the-little-schemer-clj.chapter4]
        [the-little-schemer-clj.chapter2]
        [the-little-schemer-clj.chapter3]))

(defn numbered?
  [e]
  (cond
    (atom? e) (number? e)
    (and (numbered? (first e))
         (numbered? (first (rest (rest e))))
         (or (= (first (rest e)) '*)
             (= (first (rest e)) '+)
             (= (first (rest e)) '-)
             (= (first (rest e)) '/)
             (= (first (rest e)) 'e))) true
    :else false))

(defn value
  [nexp]
  (cond
    (atom? nexp) nexp
    (= (first (rest nexp)) '*) (* (value (first nexp)) (value (first (rest (rest nexp)))))
    (= (first (rest nexp)) '+) (+ (value (first nexp)) (value (first (rest (rest nexp)))))
    (= (first (rest nexp)) '-) (- (value (first nexp)) (value (first (rest (rest nexp)))))
    (= (first (rest nexp)) '/) (/ (value (first nexp)) (value (first (rest (rest nexp)))))
    (= (first (rest nexp)) 'exp) (exp (value (first nexp)) (value (first (rest (rest nexp))))) ))

;(value '(1 + (2 + 3)))
;(value '(3 * (1 + 2)))

(defn value-v2
  [nexp]
  (cond
    (atom? nexp) nexp
    (= (first nexp) '*) (* (value (first (rest nexp))) (value (first (rest (rest nexp)))))
    (= (first nexp) '+) (+ (value (first (rest nexp))) (value (first (rest (rest nexp)))))
    (= (first nexp) '-) (- (value (first (rest nexp))) (value (first (rest (rest nexp)))))
    (= (first nexp) '/) (/ (value (first (rest nexp))) (value (first (rest (rest nexp)))))
    (= (first nexp) 'exp) (exp (value (first nexp)) (value (first (rest (rest nexp)))))))

(defn first-sub-exp
  [aexp]
  (first (rest aexp)))

(defn second-sub-exp
  [aexp]
  (first (rest (rest aexp))))

(defn operator
  [aexp]
  (first aexp))

(defn value-v3
  [nexp]
  (cond
    (atom? nexp) nexp
    (= (operator nexp) '*) (* (value (first-sub-exp nexp)) (value (second-sub-exp nexp)))
    (= (operator nexp) '+) (+ (value (first-sub-exp nexp)) (value (second-sub-exp nexp)))
    (= (operator nexp) '-) (- (value (first-sub-exp nexp)) (value (second-sub-exp nexp)))
    (= (operator nexp) '/) (/ (value (first-sub-exp nexp)) (value (second-sub-exp nexp)))
    (= (operator nexp) 'exp) (exp (value (first-sub-exp nexp)) (value (second-sub-exp nexp)))
    ))

(defn sero?
  [n]
  (empty? n))

(defn edd1
  [n]
  (cons () n))

(defn zub1
  [n]
  (rest n))

(defn splus
  [n m]
  (cond
    (sero? m) n
    :else (edd1 (splus n (zub1 m)))))

