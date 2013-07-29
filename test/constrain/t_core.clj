(ns constrain.t-core
  (:use midje.sweet)
  (:use [constrain.core]))

(facts "SameProp"
       (fact "computes error in that property"
             (error (constrain.core.Same. :a :b) {:a 1 :b 1}) => 0
             (error (constrain.core.Same. :a :b) {:a 1 :b 2}) => 1))

(facts "total-energy"
       (fact "computes sum of errors squared"
             (total-energy [(constrain.core.Same. :a :b)
                            (constrain.core.Same. :b :c)]
                           {:a 1 :b 2 :c 4}) => 5.0))

(facts "relevant-constraints"
       (fact "finds constraints that constrain the given var"
             (let [c1 (constrain.core.Same. :a :b) 
                   c2 (constrain.core.Same. :a :c)
                   c3 (constrain.core.Same. :b :c)]
               (relevant-constraints :a [c1 c2 c3] {}) => [c1 c2]
               (relevant-constraints :x [c1 c2 c3] {}) => [])))

(facts "linear-approximation"
       (fact "finds a slope and intercept for an approximation of the constraint"
             (let [c1 (constrain.core.Same. :a :b)
                   env {:a 1 :b 2}]
               (linear-approximation [c1] :a env) => (just  [(roughly -1) (roughly 2)]))))

(facts "walk-downhill"
       (fact "changes the variable to reach a local energy minimum"
             (let [c1 (constrain.core.Same. :a :b)
                   env {:a 1 :b 10}]
               (walk-downhill [c1] :a env) => (just  {:a (roughly 10) :b 10}))))
