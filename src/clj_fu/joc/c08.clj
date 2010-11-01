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