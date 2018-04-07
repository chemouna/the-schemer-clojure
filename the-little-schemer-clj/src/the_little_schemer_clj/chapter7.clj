
(ns the-little-schemer-clj.chapter6
  (:use [the-little-schemer-clj.chapter4]
        [the-little-schemer-clj.chapter2]
        [the-little-schemer-clj.chapter3]))

(defn set?
  [lat]
  (cond
    (empty? lat) true
    (= (occur (first lat) (rest lat)) 0) (set? (rest lat))
    :else false))

(defn set2?
  [lat]
  (cond
    (empty? lat) true
    (member? (first lat) (rest lat)) false
    :else (set2? (rest lat))))

(defn makeset
  [lat]
  (cond
    (empty? lat) lat
    (member? (first lat) (rest lat)) (makeset (rest lat))
    :else (cons (first lat) (makeset (rest lat))) ))

(defn makeset2
  [lat]
  (cond
    (empty? lat) lat
    :else (cons (first lat) (makeset2 (multirember (first lat) (rest lat))))))

(defn subset?
  [s1 s2]
  (cond
    (empty? s1) true
    :else (and (member? (first s1) s2) (subset? (rest s1) s2))))

(defn eqset?
  [s1 s2]
  (and (subset? s1 s2) (subset? s2 s1)))

(defn intersect?
  [s1 s2]
  (cond
    (empty? s1) false
    :else (or (member? (first s1) s2) (intersect? (rest s1) s2))))

(defn intersect
  [s1 s2]
   (cond
    (empty? s1) ()
    (member? (first s1) s2) (cons (first s1) (intersect (rest s1) s2))
    :else (intersect (rest s1) s2)))

(defn union
  [s1 s2]
  (cond
    (empty? s1) s2
    (member? (first s1) s2) (union (rest s1) s2)
    :else (cons (first s1) (union (rest s1) s2))))

(defn diff
  [s1 s2]
  (cond
    (empty? s1) ()
    (member? (first s1) s2) (diff (rest s1) s2)
    :else (cons (first s1) (diff (rest s1) s2))))

(comment "
;; wrong first attempt :(
(defn intersectall
  [lset]
  (cond

    (member* (first (first lset)) (rest lset)) (first (first lset))
    :else (intersectall (cons (rest (first lset)) (rest lset)))))
")

(defn intersectall
  [lset]
  (cond
    (empty? (rest lset)) (first lset)
    :else (intersect (first lset) (intersectall (rest lset)))))

;(intersectall '((1 2 3) (4 8 2) (9 7 2 3)))

(defn a-pair?
  [x]
  (cond
    (atom? x) false
    (empty? x) false
    (empty? (rest x)) false
    (empty? (rest (rest x))) true
    :else false))

; (a-pair? '())
; (a-pair? '(1 2))
; (a-pair? '(1 2 3))
; (a-pair? 2)

(defn build
  [s1 s2]
  (cons s1 (cons s2 '())))

(defn third
  [l]
  (first (rest (rest l))))

(defn fun?
  [rel]
  (set? (firsts rel)))

(defn revrel
  [rel]
  (cond
    (empty? rel) rel
    :else (cons (build (second (first rel)) (first (first rel))) (revrel (rest rel)))))

(defn revpair
  [pair]
  (build (second pair) (first pair)))

(defn revrel2
  [rel]
  (cond
    (empty? rel) rel
    :else (cons (revpair (first rel)) (revrel2 (rest rel)))))

(defn seconds
  [coll]
  (cond
    (empty? coll) ()
    :else (cons (second (first coll))
                (seconds (rest coll)))))

(defn fullfun?
  [fun]
  (set? (seconds fun)))

(defn one-to-one
  [fun]
  (fun? (revrel fun)))





