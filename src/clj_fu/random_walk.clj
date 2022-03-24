(ns clj_fu.random_walk
  (:require [com.hypirion.clj-xchart :as c]))

(defn next-step [e]
  (if (> (rand 1) 0.5)
    (inc e)
    (dec e)))

(def r 1000000)

(defn f [pv]
  (lazy-seq
   (cons pv
         (f (+ 0.1 pv)))))

(c/view
 (c/xy-chart
  (into {}
        (map (fn [n]
               [(str "rw_" n)
                {:x (range r)
                 :y (take r (f 1))}])
               (range 10)))
  
  {:title "x"
   :x-axis {:title "xs"}
   }))





