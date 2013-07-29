(ns constrain.core)

(defn abs [v]
  (if (neg? v) (- v) v))

(defprotocol Constraint
  (constrained [self env] "The names of the constrained variables")
  (changeable [self env] "The names of the variables that can change from this constraint")
  (error [self env] "The numerical error in meeting this constraint given values in env.")
  (degrees-of-freedom-removed [self] "The number of degrees of freedom removed by this constraint"))

(defrecord Same [a b]
  Constraint
  (constrained [self env] #{a b})
  (changeable [self env] #{a b})
  (degrees-of-freedom-removed [self] 1)
  (error [self env]
    (abs (- (env a) (env b)))))

(defrecord Colinear [x1 y1 x2 y2 x3 y3]
  Constraint
  (degrees-of-freedom-removed [self] 1)
  (error [self env]
    (1)))

(defn total-energy [constraints env]
  (apply + (map #(Math/pow (error % env) 2) constraints)))

(defn relevant-constraints [var constraints env]
  (filter #((set (constrained % env)) var) constraints))

(defn linear-approximation [constraints var env]
  (let [start-value (env var)
        cs (relevant-constraints var constraints env)
        start-energy (total-energy cs env)
        energy-inc (total-energy cs (update-in env [var] - 0.1))
        energy-dec (total-energy cs (update-in env [var] + 0.1))
        slope (/ (- energy-inc energy-dec) 0.2)
        intercept (- start-energy (* slope start-value))]
    [intercept slope]))

(defn walk-downhill [constraints var env]
  (let [[intercept slope] (linear-approximation constraints var env)]
    (if (< (abs slope) 0.1)
      env
      (recur constraints var (update-in env [var]
                                        (if (neg? slope) - +)
                                        0.1)))))


