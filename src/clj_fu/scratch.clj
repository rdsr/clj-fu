(ns scratch
  (:require [clojure.data.csv :as c]
            [clojure.java.io :as io]))

(defn csv->map [data]
  (let [ks (->> data
                first ;; header
                (map keyword)
                repeat)]
    (map zipmap ks (rest data))))

(defn read-file [f]
  (c/read-csv (io/reader f)))

(def gp "/Users/rratti/tmp/gp.csv")
(def bw "/Users/rratti/tmp/bw.csv")
(first (read-file gp))
(first (read-file bw))

(defn idx-by-name [rows]
  (reduce (fn [m row]
           (assoc m (:name row) row))
         {}
         rows))

(def gpd (csv->map (read-file gp)))
(def bwd (csv->map (read-file bw)))

(idx-by-name gpd)
(idx-by-name bwd)



(map :name gpd)
(map :name bwd)

(doseq [e gpd]
  (println (:name e)))

(doseq [e bwd]
  (println (:name e)))



(defn pangram? [s]
  (or (empty? s)
      (= (set "abcdefghijklmnopqrstuvwxyz")
         (set (->> (.toLowerCase s)
                   (filter (fn [c] (Character/isLetter c))))))))

(pangram? "The quick brown fox jumps over the lazy dog.")

(defn digits [x]
  (loop [x x r ()]
    (if (= x 0)
      r
      (recur
     (quot x 10)
     (conj r (rem x 10))))))

(defn persistence [n]
  (->> n
       (iterate (fn [x] (apply * (digits x))))
       (partition 2 1)
       (take-while (fn [[a b]] (> a 9)))
       count))

(defn cs [ns]
  (if (empty? ns)
    ()
    (lazy-seq
     (let [e (first ns)]
       (cons e
             (map #(+ % e) (cs (rest ns))))))))

(defn save [sizes hd]
  (count (take-while #(<= % hd) (cs sizes))))

(defn strip-comments [text comment-symbols]
  (let [cs (set comment-symbols)
        lines (.split text "\n")]
    (clojure.string/join
     (for [l lines]
       (let [ci (->> cs
                   (map #(.indexOf l %))
                   (filter #(>= % 0)))]
         (if (empty? ci) l
             (->> ci
                  (apply min) 
                  (.substring l 0)
                  clojure.string/trim))))
     "\n")))



