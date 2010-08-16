(ns simply.integer)

(defn integer [x] (java.lang.Integer/parseInt (if (keyword? x) (name x) (str x))))
(defn dividable? [n m] (-> n (mod m) zero?))
