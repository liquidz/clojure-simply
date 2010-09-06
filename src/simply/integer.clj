(ns simply.integer)

(defprotocol ConvertInteger
  (int [this])
  )

(extend String ConvertInteger
  {:int (fn [this] (Integer/parseInt this))})
(extend Keyword ConvertInteger
  {:int (fn [this] (Integer/parseInt (name this)))})
;(extend Double ConvertInteger
;  {:int (fn [this] (clojure.core/int this))})

