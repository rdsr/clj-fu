(ns clj-fu.play.transducers)

(def data (range 0 10))

(defn -map [f coll]
  (reduce
   (fn [acc e]
     (conj acc (f e)))
   []
   coll))

(-map dec data)

(defn -filter [f coll]
  (reduce
   (fn [acc e]
     (if (f e)
       (conj acc e)
       acc))
   []
   coll))

(-filter even? data)

(->> data
     (map inc)
     (filter even?))


(defn -map-step [f]
  (fn [rf]
    (fn [result e]
      (rf result (f e)))))


(def map-transducer (-map-step inc))
(reduce (map-transducer conj) [] data)

(defn -filter-step [f]
  (fn [rf]
    (fn [result e]
      (if (f e)
        (rf result e)
        result))))

(def filter-transducer (-filter-step even?))
(reduce (filter-transducer conj) [] data)

(reduce ((comp filter-transducer map-transducer) conj) [] data)

(((-filter-step identity) conj) [1 2 3] 1)

(def xform
  (comp
   (-filter-step even?)
   (-map-step inc)))

(reduce (xform conj) [] (range 10))

(defn -transduce [xform rf initial-value data]
  (reduce (xform rf) initial-value data))
(-transduce xform conj [] (range 10))

;; caeser cipher
(defn char->int [^Character c]
  (-> c char int))
(defn not-vowel [^Character c] (not (#{\a \e \i \o \u} c)))
(defn ascii [^Character c] (< (char->int c) 127))
(defn not-upcase [^Character c] (not (Character/isUpperCase c)))
(defn rotate [^Character c n]
  (let [n  (rem n 26)                                       ; between 0 - 26 letters
        lb (int \a)
        ub (int \z)]
    (-> c
        int
        (- lb) (+ n) (rem 26)
        (+ lb)
        char)))


;; reducing fn -> assoc

(defn caesar-count [s cipher]
  (let [xform (comp (-filter-step (fn [^Character c]
                                    (and (not-vowel c)
                                         (ascii c)
                                         (not-upcase c))))
                    (-map-step #(rotate % cipher)))
        fq (fn [m c]
             (assoc m c
                      (+ 1 (m c 0))))]
   (reduce (xform fq) {} s)))

(caesar-count "abc" 0)
(caesar-count "abc" 1)
(caesar-count "hello world" 0)
(caesar-count "hello world" 13)

(defn -mapcat-step [f]
  (fn [rf]
    (fn [result x]
      (reduce rf result (f x)))))

(defn twins [x] [x x])
(reduce ((-mapcat-step twins) conj) [] (range 10))


(defn taking [n]
  (fn [rf]
    (let [n (volatile! n)]
     (fn [ires e]
       (if (>= (vswap! n dec) 0)
        (rf ires e)
        ires)))))

(reduce ((taking 3) conj) [] (range 2))
(reduce ((taking 3) conj) [] (range 10))
