(ns simply.def)

; =defnk
(defn split-map-other [col]
  (let [maps (reduce concat () (take-while #(-> % first keyword?) (partition 2 col)))
        other (drop (count maps) col)]
    [(apply hash-map maps) other]
    )
  )

(defmacro fnk [args & body]
  (let [
        [normal-args [_ extra]] (split-with #(not (= % '&)) args)
        [fixed-args k-args] (split-with symbol? normal-args)
        s-args (map #(if (keyword? %) (-> % name symbol) %) k-args)
        default-map (apply hash-map s-args)
        extra-name (if (nil? extra) (gensym) extra)
        [condition-map & rest-body] (if (and (rest body) (-> body first map?)) body (cons () body))
        ]
    `(fn [~@fixed-args & more#]
       ~condition-map
       (let [[kvs# ~extra-name] (split-map-other more#)
             {:keys [~@(filter symbol? s-args)] :or ~default-map} kvs#]
         ~@rest-body
         )
       )
    )
  )

(defmacro defnk [fname & frest]
  `(def ~fname (fnk ~@frest))
  )

(defmacro defnk- [name & decls]
  (list* `defnk (with-meta name (assoc (meta name) :private true)) decls)
  )
