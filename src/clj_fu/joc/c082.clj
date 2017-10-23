(ns joc.c082)

(defmacro domain [name & body]
  `{:tag :domain,
    :attrs {:name (str '~name)},
    :content [~@body]})

(declare handle-things)

(defmacro grouping [name & body]
  `{:tag :grouping,
    :attrs {:name (str '~name)},
    :content [~@(handle-things body)]})

(declare grok-attrs grok-props)

(defn handle-things [things]
  (for [t things]
    {:tag :thing,
     :attrs (grok-attrs (take-while (comp not vector?) t))
     :content (if-let [c (grok-props (drop-while (comp not vector?) t))]
                [c]
                [])}))

(defn grok-attrs [attrs]
  (into {:name (str (first attrs))}
        (for [a (rest attrs)]
          (cond
            (list? a) [:isa (str (second a))]
            (string? a) [:comment a]))))

(defn grok-props [props]
  (when props
    {:tag :properties, :attrs nil,
     :content (apply vector (for [p props]
                              {:tag :property,
                               :attrs {:name (str (first p))},
                               :content nil}))}))

(domain man-vs-monster
        (grouping people                  ;; #: Group of people
                  (Human "A stock human") ;; #: One kind of person

                  (Man (isa Human) ;; #: Another kind of person
                       "A man, baby"
                       [name]
                       [has-beard?]))
        (grouping monsters    ;; #: Group of monsters
                  (Chupacabra ;; #: One kind of monster
                   "A fierce, yet elusive creature"
                   [eats-goats?])))

