(ns protocols)

;; not interfaces
;; not monkey patches
;; not wrappers
;; not mixins
;; no injection
;; no implicit conversion
;; no dirty tricks

;; motivations
;; write to abstractions
;; clojure in clojure
;; solve the expression problems

(defprotocol Grow
  "a simple protocol"
  (grow [self] "growing...")) ;; self is the type implementing the protocol itself

;; defprotocol will automatically generate a corresponding interface with the
;; same name as the protocol

;; on declaration, two new vars are created
;; 1. Grow (The protocol itself) and grow
;; (the polymorphic function which gets
;; called when we execute it on an implementation
;; of Grow

(defrecord Tree [kind description])  ;; a type with two fields
(def banyan (Tree. "Banyan" "a banyan tree"))

(:kind banyan) ;; can be used as a map
(def another-banyan (assoc banyan :age 10000))

(try
  (banyan :kind)
  (catch Exception _
    "but not all map functionality is supported"))

;; implementing the protocol
;; 1st method, "inlining"
(defrecord Human [name age]
  Grow
  (grow [self]
        (Human. (:name self)
                (+ (:age self) 1))))

(def you (Human. "Jack" 10))
(def older-you (grow you))
(type you)
(class you)

;; what if we want to "grow" implementations of type Tree?
;; second method, "extending a type"
(extend-type Tree
  Grow
  (grow [self] "getting wiser"))
(grow banyan)
;; This is necessary for cases where you do not
;; have the source code for the already implemented Type

;; we may require an anonymous object which implements a protocol/interface or Object,
;; this object may not contain any type information
(grow (reify
        Grow
        (grow [self] "growing...")))

 (let [age 100]  ;; can access surrounding lexical scope
   (grow
    (reify Grow
         (grow [self] (str "grew by " age " years")))))

;; use reify instead of proxy where-ever possible
;; when defrecord is created, Clojure implements interface like persistent maps, hashcode, keyword accessors etc,
;; whereas for deftype none of these interface are added by default
;; (deftype may be required for cases where we want a type with mutable fields)
