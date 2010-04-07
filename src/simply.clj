(ns simply
  (:import (java.io BufferedReader FileReader))
  )

(defmacro p
  ([v] `(do (println ~v) ~v))
  ([l & args]
   `(let [x# (~l ~@args)]
      (println x#)
      x#
      )
   )
  )

(defn !=
  ([x] true)
  ([x y] (not (= x y)))
  ([x y & more] (and (= x y) (apply = more)))
  )

(def ++ inc)
(def -- dec)

(defn foreach [f & seq-exprs]
  {:pre [(every? seq? seq-exprs)]}
  (doseq [seq seq-exprs]
    (doseq [x seq] (f x))
    )
  )

(defn fold [f val seq]
  (loop [res val, ls seq]
    (if (empty? ls)
      res
      (recur (f (first ls) res) (rest ls))
      )
    )
  )

(defn str-convert-encode [encoding & strs]
  {:pre [(string? encoding)]
   :post [(string? %)]
   }
  (String. (.getBytes (apply str strs) encoding))
  )
(def to-utf8 (partial str-convert-encode "UTF-8"))
(def to-euc (partial str-convert-encode "EUC-JP"))
(def to-sjis (partial str-convert-encode "Shift_JIS"))

