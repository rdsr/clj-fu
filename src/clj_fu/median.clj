(ns clj-fu.median)

;; finding the median of two sorted vectors in O(nlgn)

(defn first-half [v]
  (subvec v 0 (/ (count v) 2)))

(defn second-half [v]
  (let [sz (count v)]
    (if (even? sz)
      (subvec v (quot sz 2))
      (subvec v (inc (quot sz 2))))))

(defn median [v]
  (let [sz (count v)]
  (if (even? sz)
    (v (dec (quot sz 2)))
    (v (quot sz 2)))))

(defn median-2v
  "finds the median of two sorted vectors"
  [u v]
  (let [m (median u)
        n (median v)]
    (cond
     (= m n) m
     (= (count u) 1) (if (< m n) m n)
     (< m n) (median-2v (second-half u) (first-half v))
     (> m n) (median-2v (second-half v) (first-half u)))))

(defn make-vec [n]
  (into [] (sort (map #(rand-int %) (range 0 n)))))


(median-2v (make-vec 1000000) (make-vec 1000000))
