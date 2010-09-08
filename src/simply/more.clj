(ns simply.more)


(defmacro defmethods [name param & class-body]
  (cons 'do
        (reduce
          (fn [res [class & body]]
            (if (vector? class)
              (concat res (map #(concat (list 'defmethod name % param) body) class))
              (cons (concat (list 'defmethod name class param) body) res)
              )
            )
          ()
          class-body)
        )
  )

(defmulti ++ (fn [& _] (-> _ first class)))
(defmethods ++ [& args]
  ([Integer Double BigInteger clojure.lang.Ratio] (apply + args))
  (String (apply str args))
  (clojure.lang.IPersistentList (apply concat args))
  (clojure.lang.IPersistentVector (vec (apply concat args)))
  (clojure.lang.IPersistentMap (apply assoc (cons (first args) (flatten (map #(reduce concat () %) (rest args))))))
  (clojure.lang.Fn (apply comp args))
  )
