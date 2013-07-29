(ns constrain.t-core
  (:use midje.sweet)
  (:use [constrain.core]))

(facts "Same"
       (fact "computes error in that property"
             (error (constrain.core.Same. :a :b) {:a 1 :b 1}) => 0
             (error (constrain.core.Same. :a :b) {:a 1 :b 2}) => 1))

(facts "Colinear"
       (fact "computes distance of point from line"
             (let [c1 (constrain.core.Colinear. :x1 :y1 :x2 :y2 :x3 :y3)
                   env {:x1 0 :y1 0 :x2 1 :y2 1 :x3 2 :y3 3}]
               (error c1 env) => 1)))

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
       (fact "changes all variables to reach a local energy minimum"
             (let [c1 (constrain.core.Same. :a :b)
                   c2 (constrain.core.Same. :a :c)
                   env {:a 10 :b 50 :c 20}]
               (walk-downhill [c1 c2] [:a :b :c] env) => (just {:a (roughly 20.1)
                                                                :b (roughly 20.1)
                                                                :c (roughly 20.1)}))))