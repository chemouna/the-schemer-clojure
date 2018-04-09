
(ns the-little-schemer-clj.chapter10
  (:use [the-little-schemer-clj.chapter2]
        [the-little-schemer-clj.chapter3]
        [the-little-schemer-clj.chapter4]
        [the-little-schemer-clj.chapter7]
        [clojure.tools.trace :as trace]))

(def new-entry build)

(defn lookup-in-entry-help
  [name names values entry-f]
  (cond
    (empty? names) (entry-f name)
    (= name (first names)) (first values)
    :else (lookup-in-entry-help name (rest names) (rest values) entry-f)))

(defn lookup-in-entry
  [name entry entry-f]
  (lookup-in-entry-help name (first entry)
                             (second entry)
                              entry-f))

(def extend-table cons)

(defn lookup-in-table
  [name table table-f]
  (cond
    (empty? table) (table-f name)
    :else (lookup-in-entry name (first table) (fn [name]
                                                (lookup-in-table (rest table) table-f)))))

(defn *const
  [e table]
  (cond
   (number? e) e
   (= e :t) true
   (= e :f) false
   :else (build 'primitive e)))

(def text-of second)

(defn *quote
  [e table]
  (text-of e))

(defn initial-table
  [name]
  (first '()))

(defn *identifier
  [e table]
  (lookup-in-table e table initial-table))

(defn atom-to-action
  [e]
  (cond
    (number? e) *const
    (= e :f) *const
    (= e :t) *const
    (= e 'cons) *const
    (= e 'car) *const
    (= e 'cdr) *const
    (= e 'empty?) *const
    (= e 'eq?) *const
    (= e 'atom?) *const
    (= e 'zero?) *const
    (= e 'add1) *const
    (= e 'sub1) *const
    (= e 'number?) *const
    :else *identifier))

(defn *lambda
  [e table]
  (build 'non-primitive
         (cons table (rest e))))

(def table-of first)

(def formals-of second)

(def body-of third)

(def question-of first)

(def answer-of second)

(defn else?
  [x]
  (cond
    (atom? x) (= x 'else)
    :else false ))

(def cond-lines-of rest)

(defn meaning
  [e table]
  ((expression-to-action e) e table))

(defn evcon
  [lines table]
  (cond
    (else? (question-of (first lines))) (meaning (answer-of (first lines)) table)
    (meaning (question-of (first lines)) table) (meaning (answer-of (first lines) table))
    :else (recur (rest lines) table)))

(defn *cond
  [e table]
  (evcon (cond-lines-of e) table))

;(*cond (cond
;         (coffee) klatsch
;         :else "party")
;       '(((coffee) (true)) ((klatsch party) (5 (6)))))

(defn list-to-action
  [e]
  (cond
    (atom? (first e)) (cond
                        (= (first e) 'quote) *quote
                        (= (first e) 'lambda) *lambda
                        (= (first e) 'cond) *cond
                        :else  *application)))

(defn value
  [e]
  (meaning e '()))
