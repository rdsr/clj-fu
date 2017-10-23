(ns clj-fu.programming-clojure.c04)


;(def r (service 1 2 (fn [& xs]
;                     (println "hello")
;                      (apply + xs))))

;; convert an async call to a sync call
;; some webservice exposive callbacks
;; we can't change that code
(defn service [a b fn]
  (future (fn a b)))

(defn sync-fn
  [async-fn]
  (fn [& args]
    (let [p (promise)]
      (apply async-fn (conj (vec args) #(deliver p %&)))
      @p)))

(defn phone-numbers [string]
  (re-seq #"(\d{3})[\.-]?(\d{3})[\.-]?(\d{4})" string))

(def files (repeat 100000 (apply str
                                 (concat (repeat 100000 \space)
                                         "Sunil: 617.555.2937, Betty: 508.555.2218"))))

(time (dorun (pmap phone-numbers files)))

;; concurrency helper utils
(defmacro futures [& exprs]
  (vec
    (for [e exprs]
     `(future ~e))))

(defmacro wait-futures [& args]
  `(doseq [f# (futures ~@args)]
     @f#))

;; atoms
(def s (atom {:name "Sarah"}))
(swap! s update-in [:name] keyword)

(def xs (atom #{1,2,3}))
(wait-futures (swap! xs (fn [v] (Thread/sleep 250) (println "Trying 4") (conj v 4)))
              (swap! xs (fn [v] (Thread/sleep 350) (println "Trying 5") (conj v 5)))
              (swap! xs (fn [v] (Thread/sleep 150) (println "Trying 6") (conj v 6)))
              (swap! xs (fn [v] (Thread/sleep 550) (println "Trying 7") (conj v 7)))
              (swap! xs (fn [v] (Thread/sleep 550) (println "Trying 8") (conj v 8)))
              (swap! xs (fn [v] (Thread/sleep 550) (println "Trying 9") (conj v 9)))
              (swap! xs (fn [v] (Thread/sleep 550) (println "Trying 10") (conj v 10)))
              (swap! xs (fn [v] (Thread/sleep 550) (println "Trying 11") (conj v 11)))
              (swap! xs (fn [v] (Thread/sleep 550) (println "Trying 12") (conj v 12)))
              (swap! xs (fn [v] (Thread/sleep 550) (println "Trying 13") (conj v 13)))
              (swap! xs (fn [v] (Thread/sleep 550) (println "Trying 14") (conj v 14)))
              )
(compare-and-set! xs @xs "hello")
(reset! xs [])

(defn character [name & {:as opts}]
  (ref (merge {:name name :items #{} :health 500} opts)))

(def smaug (character "Smaug" :strength 500 :health 400 :items (set (range 50))))
(def bilbo (character "Bilbo" :strength 100 :health 100))
(def gandalf (character "Gandalf" :strength 200 :health 200))

(defn loot [from to]
  (dosync
    (when-let [item (-> @from :items first)]
      (alter to update-in [:items] conj item)
      (alter from update-in [:items] disj item))))

(wait-futures
  (while (loot smaug bilbo))
  (while (loot smaug gandalf)))

(map (comp count :items deref) [bilbo gandalf])
(filter (:items bilbo) (:items gandalf))