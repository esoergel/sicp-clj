(ns sicp-clj.core
  "This uses macros to expand and modify Clojure's syntax to support
   Scheme syntax.  Mutability is not yet supported"
  (:require [clojure.string :as s]
            [clojure.edn :as edn]
            [clojure.repl :refer (doc)]))
 
;; scheme-clj.core> (in-ns 'scheme-clj.clj-extended)

(defmacro define
  [variable & body]
  (if (list? variable)
    (let [[name & args] variable]
      (concat  (list 'defn
                     name
                     (into [] args))
               body))
    (conj body variable 'def)))

(defn expand-cond
  [clauses]
  (if (empty? clauses)
    false
    (let [[[predicate clause] & others] clauses
          predicate (if (= predicate 'else) true predicate)]
      `(if ~predicate
         ~clause
         ~(expand-cond others)))))

(defmacro cond
  [& expressions]
  (expand-cond expressions))

(defn cons
  "An SICP style pair defined by functions only"
  [a b]
  (fn [operation]
    (clojure.core/cond
     (= operation 'car) a
     (= operation 'cdr) b)))
(defn car [pair] (pair 'car))
(defn cdr [pair] (pair 'cdr))
;; Not sure if this is possible (def pair? ???)

(defrecord Pair [fst snd])
(defn cons [a b] (atom (Pair. (atom a) (atom b))))
(defn pair? [p] (instance? Pair @p))
(defn assert-pair [p] (assert (pair? p) "Object is not a pair"))
(defn car [p] (do (assert-pair p) @(:fst @p)))
(defn cdr [p] (do (assert-pair p) @(:snd @p)))
(defn set-car! [p x] (do assert-pair p) (reset! (:fst @p) x))
(defn set-cdr! [p x] (do assert-pair p) (reset! (:snd @p) x))

(def string str)
(def string-append str)
(def display print)
