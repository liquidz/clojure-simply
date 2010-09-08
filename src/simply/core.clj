(ns simply.core)

(defmacro !
  ([v] `(not ~v))
  ([v & more] `(not (~v ~@more)))
  )

(defmacro fnk [params & body]
  (let [key->sym (comp symbol name)
        [fixed-named-args [_ extra]] (split-with #(not (= % '&)) params)
        [fixed-args named-args] (split-with symbol? fixed-named-args)
        name-syms (->> named-args (filter keyword?) (map key->sym))
        named-map (apply hash-map (map #(if (keyword? %) (key->sym %) %) named-args))
        extra-arg-name (if (nil? extra) (gensym) extra)
        [condition-map & body*] (if (-> body first map?) body (cons {} body))
        ]
    `(fn [~@fixed-args & args#]
       ~condition-map
       (let [k# (-> (split-with (comp keyword? first) (partition 2 args#)) first flatten)
             r# (drop (count k#) args#)
             {:keys [~@name-syms] :or ~named-map} k#
             ~extra-arg-name r#
             ]
         ~@body*
         )
       )
    )
  )
(defmacro defnk [name & decls] `(def ~name (fnk ~@decls)))
(defmacro defnk- [name & decls] (list* `defnk (with-meta name (assoc (meta name) :private true)) decls))

(defn foreach [f & colls]
  (if (> (count colls) 1)
    (doseq [x (partition 2 (apply interleave colls))] (apply f x))
    (doseq [x (first colls)] (f x))
    )
  )

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
