(ns ex2n1-specwrap)

(def gas-constant 8.3144621); J/(K*mol)

(defn expt [a b] (Math/pow a b))

(defn gas-law-volume
  [pressure temperature amount]
  (/ (* amount gas-constant temperature) pressure))

(defn sphere-radius [volume] (expt (/ volume (* (/ 4 3) Math/PI)) (/ 1 3)))

(def meter 1)
(def cm 0.01)
(def inch 0.0254)

(let [meter 1 inch 0.0254] (* 5 (eval '(expt inch 3))))

; 5" * 0.0254 in/m = 0.0254 m, 0.0254 m / 0.01 m/cm = 2.54 cm

(defn convert
  [input actual-unit expected-unit]
  ;; If actual-unit evaluates to a number, eval it and multiply input by it.
  ;; If not, look in the conversion table for a fn that can convert from that
  ;; unit to the standard unit.
  ;; If expected-unit evaluates to a number, eval it and divide intermediary
  ;; value by it.
  ;; If not, look in the conversion table for a fn that convert from a standard
  ;; unit to that unit.
  ;; ↑ Only works if direct conversions are defined between all units. For
  ;; conversions with intermediate steps, see the sketch of my previous exercise
  ;; solution. Or the algebra shown in the book.
  ;; No. It does work as long as we have a conversation between every unit and a
  ;; standard unitNo. It does work as long as we have a conversation between
  ;; every unit and a standard unit.
)

;; FIXME: I failed to distinguish between output and inputs.
(defn function-specializer
  [f & expected-units]
  (fn specializer [& actual-units]
    {:pre [(= (count expected-units) (count actual-units))]}
    (fn specialized [& inputs]
      {:pre [(= (count actual-units) (count inputs))]}
      (apply f (map convert inputs actual-units expected-units)))))

;; Define what units gas-law-volume expects.
(def make-specialized-gas-law-volume
  (function-specializer gas-law-volume
                        '(expt meter 3)
                        '(/ newton (expt meter 2))
                        'kelvin
                        'mole))

(def conventional-gas-law-volume
  (make-specialized-gas-law-volume '(expt inch 3)
                                   '(/ pound (expt inch 2))
                                   'fahrenheit
                                   'mole))