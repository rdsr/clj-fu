(ns clj-fu.joc.c07
  (:use [clojure.test]))

;; 7.1 pure functions
;; 1.  always has the same value for expected args.
;; 2.  has no observable side-effects

(def plays [{:band "Burial", :plays 979, :loved 9}
            {:band "Eno", :plays 2333, :loved 15}
            {:band "Bill Evans", :plays 979, :loved 9}
            {:band "Magma", :plays 2665, :loved 31}])

(defn keys-apply [f ks m]
  (let [only (select-keys m ks)]
    (zipmap ks (map f (vals only)))))

(defn manip-map [f ks m]
  (conj m (keys-apply f ks m)))

(defn manip-map-v2
  "figuring out other ways"
  [f ks m]
  (reduce (fn [m k]
            (assoc m k (f (m k))))
          m ks))

;; named arguments in clojure
(defn slope
  [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]}}]  ;; destructing it as a map with default values for keys p1 and p2
  (if (zero? (- (p2 0) (p1 0)))
    0.0
    (float (/ (- (p2 1) (p1 1))
              (- (p2 0) (p1 0))))))

(slope :p1 [1 2] :p2 [0 1]) ;; even number of args
(slope :p1 [1 1])           ;; use default value for p2

;; pre and post conditions, adding constraints to function definitions
(defn slope
  [p1 p2]
  {:pre  [(not= (p2 0) (p1 0)) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))

(is (thrown? AssertionError (slope [1 2] [1 3])))

;; constraints need not be added at the time of function definition
;; but can be added in later on a wrapper functions, moreover the
;; original function can be more general, and our wrapper function
;; can add domain specific constraints. (aspects)

;; 7.2 closures
;; closing over a mutable value
(defn add-and-get []
  (let [ai (java.util.concurrent.atomic.AtomicInteger.)]
    (fn [y]
      (.addAndGet ai y))))

(def ag (add-and-get))
(is (= 2 (ag 1)))
(is (= 3 (ag 1)))
(is (= 4 (ag 1)))

(defn times-n [n] (fn [x] (* x n)))
(def times-two (times-n 2))
(is (= 6 (times-two 3)))


;; anywhere a function is expected, a closure can be used instead

(def bearings {:north [0 1] :east [1 0] :south [-1 0] :west [0 -1]})
(def turn {:north [:west :east] :east [:north :south] :south [:east :west] :west [:south :north]})

(defn bot
  [x y bearing]
  {:coords     [x y]
   :bearing    bearing
   :forward    (fn []
                 (bot (+ x ((bearing bearings) 0))
                      (+ y ((bearing bearings) 1))
                      bearing))
   :turn-left  (fn [] (bot x y ((bearing turn) 0)))
   :turn-right (fn [] (bot x y ((bearing turn) 1)))})

(is :west (-> (bot 0 0 :north) :turn-left .invoke :bearing))
(is [0 1] (-> (bot 0 0 :north) :forward .invoke :coords))

;; since functions are contained within objects themselves, simple ploymorphism can be achieved!!
;; e.g the code below create a bot object, which reverses on calling forward, turns-right when
;; asked to turn left and vice-versa.

(defn mirror-bot
  [x y bearing]
  {:coords     [x y]
   :bearing    bearing
   :forward    (fn []
                 (bot (- x ((bearing bearings) 0))
                      (- y ((bearing bearings) 1))
                      bearing))
   :turn-left  (fn [] (bot x y ((bearing turn) 1)))
   :turn-right (fn [] (bot x y ((bearing turn) 0)))})

(is (= :east (-> (mirror-bot 0 0 :north) :turn-left .invoke :bearing)))
(is (= [0 -1] (-> (mirror-bot 0 0 :north) :forward .invoke :coords)))

;; todo: achieve this through reify
;; note: each fn definition gets its own class

;; todo: A* implementation