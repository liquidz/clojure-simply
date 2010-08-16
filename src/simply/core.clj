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

