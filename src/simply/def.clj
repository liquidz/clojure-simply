(ns simply.def)

; =defnk
(defn split-kv-extra [col]
  (loop [ls col, m {}]
    (cond
      (empty? ls) [m ()]
      (-> ls first keyword?) (recur (-> ls rest rest) (assoc m (first ls) (second ls)))
      :else [m ls]
      )
    )
  )

(defmacro fnk [args & body]
  (let [[fixed-args k-args] (split-with symbol? args)
        [k-args2 extra] (split-with #(not (= % '&)) k-args)
        s-args (map #(if (keyword? %) (-> % name symbol) %) k-args2)
        extra-name (if (empty? extra) '_extra_ (-> extra second symbol))
        keywords (filter symbol? s-args)
        default-map (apply hash-map s-args)
        [condition-map & rest-body] (if (and (rest body) (map? (first body))) body (cons () body))
        ]
    `(fn [~@fixed-args & more#]
       ~condition-map
       (let [[kvs# ~extra-name] (split-kv-extra more#)
             {:keys [~@keywords] :or ~default-map} kvs#
             ]
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
