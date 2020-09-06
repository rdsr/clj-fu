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



