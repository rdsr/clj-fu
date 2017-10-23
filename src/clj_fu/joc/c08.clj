(ns joc.c08
  (:require [clojure.test :as test]))

(defn contextual-eval
  "JoC version"
  [ctx expr]
  (eval
   `(let ~(vec (mapcat #(list % `('~ctx '~%)) (keys ctx)))
      ~expr)))

(test/is (= 0 (contextual-eval
               {'a 1, 'b 2}
               '(let [b -1] (+ a b)))))


(defn contextual-eval2
  "JOC 2 version"
  [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k v]) ctx)]
      ~expr)))

(contextual-eval2 {'a map, 'b 2} '(a (fn [x] (+ x 1)) [b]))
(contextual-eval2 '{a 1, b 2} '(let [b 1000] (+ a b)))

(let [x 9, y '(- x)]
  (println `y)
  (println ``y)
  (println ``~y)
  (println ``~~y)
  (contextual-eval {'x 36} ``~~y))


(defn contextual-eval-alternative
  "alternative version"
  [ctx expr]
  (eval
   `(let ~(vec (apply concat ctx))
      ~expr)))
(test/is (= 0 (contextual-eval-alternative
               {'a 1, 'b 2}
               '(let [b -1] (+ a b)))))

(defmacro contextual-eval-macro
  "trying out with defmacro"
  [ctx expr]
  `(let ~(vec (apply concat ctx))
     ~expr))
(test/is (= -1 (contextual-eval-macro {a 1, b 2} (let [b -2] (+ a b)))))

(defmacro do-until [& clauses]
  (when clauses
    (list 'when (first clauses)
          (second clauses)
          (cons 'do-until (nnext clauses)))))

(defmacro do-until
  "JOC version"
  [& clauses]
  (when clauses
    (list 'clojure.core/when (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (IllegalArgumentException.
                    "do-until requires an even number of forms")))
          (cons 'do-until (nnext clauses)))))

(require '[clojure.walk :as walk])
(clojure.pprint/pprint
 (walk/macroexpand-all
  '(do-until
    (even? 2) (println "2")
    (odd? 1) (println "1")
    (zero? 1) (println "failed"))))

(defmacro unless [& form]
  `(when (not ~(first form))
     ~@(rest form)))

(defmacro unless [& form]
  `(let [condition# ~(first form)]
     (when (not condition#)
       ~@(rest form))))

;; ---

(defn grok-attrs [attrs]
  (into {:name (str (first attrs))}
        (for [a (rest attrs)]
          (cond
            (string? a) [:comment a]
            (list? a) [:isa (str (second a))]))))

(defn grok-props [props]
  (when props
    {:tag     :properties
     :attrs   nil
     :content (apply vector (for [p props]
                 {:tag     :property
                  :attrs   {:name (str (first p))}
                  :content nil}))}))

(defn handle-things [things]
  (for [t things]
    {:tag     :thing
     :attrs   (grok-attrs (take-while (comp not vector?) t))
     :content (if-let [c (grok-props (drop-while (comp not vector?) t))]
                [c]
                [])}))

(defmacro grouping [name & body]
  `{:tag     :grouping
    :attrs   {:name (str '~name)}
    :content [~@(handle-things body)]})

(defmacro domain [name & body]
  `{:tag     :domain
    :attrs   {:name name}
    :content [~@body]})

(grouping monstors
          (Chupacabra
           "Some weird creature"
           [eats-goats?]))

(domain man-vs-monstor
        (grouping people
                  (Human "A Human")
                  (Man (isa Human)
                       "A man baby"
                       [name]
                       [has-beard?]))
        (grouping monstors
                  (Chupacabra
                   "Some weird creature"
                   [eats-goats?])))



(defn fnth [n]
  (apply comp first (take (dec n) (repeat rest))))

