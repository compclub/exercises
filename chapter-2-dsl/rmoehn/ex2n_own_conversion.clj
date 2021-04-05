(ns ex2n-own-conversion)

;;;; (ns unit)

(def multipliers {:milli 0.001, :centi 0.01, :kilo 1000})

;; If I want to encode compaction order, I need to use a data structure that
;; retains the order of the data literal.
;; Also, for some units, such as degrees Fahrenheit, a simple multiplication is
;; not enough. The conversion procedure must be more general.
(def conversions
  (->> {:inch (qty/convert (->q 2.54 [:centi :metre]) (->u :metre)),
        [[:kilo :gram :metre] [:second :second]] (->q 1 :newton)}
       (map (fn [k v] (->u k) v))
       (into {})))

(defn ->u
  ([numerator-spec] (->u numerator-spec []))
  ([numerator-spec denominator-spec]))

;; refer-clojure denominator numerator
(defn inverse [unit] (new (denominator unit) (numerator unit)))
(defn times [unit1 unit2] (new (concat (numerator unit1) (numerator unit2))))

;;;; (ns qty)

;;; Most of the work would be in implementing convert and compact.
;;; Also some work in figuring out the syntax and semantics of numerator-spec
;;; and denominator-spec.

(defn ->q
  ([v] (->q v [] []))
  ([v numerator-spec] (->q v numerator-spec []))
  ([v numerator-spec denominator-spec]))

(comment (->q 1 :metre) (->q 1 [:centi :metre]) (->q 1 1 :second))

(defn compact [qty]); -> Qty

(qty/convert (unit/new 1 :inch) [:milli :metre])

(defn new [value unit]); -> Qty

(defn value [qty]); -> Num

(defn unit [qty]); -> Unit

(defn convert [qty unit]); -> Qty

(defn plus
  [qty1 qty2]
  {:pre [(compatible? (unit qty1) (unit qty2))]}
  (let [converted-qty2 (convert qty2 (unit qty1))]
    (-> (new (+ (value qty) (value converted-qty2)) (unit qty1))
        compact)))

(defn minus
  [qty1 qty2]
  (-> (plus qty (new (- (value qty2)) (unit qty2)))
      compact))

(defn inverse [qty] (new (/ 1 (value qty)) (unit/inverse (unit qty))))

(defn times
  [qty1 qty2]
  (-> (new (* (value qty1) (value qty2)) (unit/times (unit qty1) (unit qty2)))
      compact))

(defn divided-by
  [qty1 qty2]
  (-> (times qty1 (inverse qty2))
      compact))