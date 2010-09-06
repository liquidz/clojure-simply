(ns simply.core)

(defmacro pp
  ([v] `(do (println ~v) ~v))
  ([l & more]
   `(let [x# (~l ~@more)]
      (println "pp)) " x#)
      x#
      )
   )
  )

(defmacro !
  ([v] `(not ~v))
  ([v & more] `(not (~v ~@more)))
  )


(def foreach #(doseq [x %2] (%1 x)))

(defn fold [f ini & coll]
  (when (-> coll first empty? not)
    (let [body (fn [res ls]
                 (if (-> ls first empty?) res
                   (recur
                     (apply f (concat (map first ls) (list res)))
                     (map #(drop 1 %) ls)
                     )
                   )
                 )]
      (body ini coll)
      )
    )
  )
