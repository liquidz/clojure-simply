(ns simply.core
  (:use [simply.list :only [insert]])
  )

; =fold
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

; =!
(defmacro !
  ([v] `(if (fn? ~v) (fn [& args#] (not (apply ~v args#))) (not ~v)))
  ([v & more] `(not (~v ~@more)))
  )

; =flatten-with-depth
(defn flatten-with-depth [n coll]
  (loop [ls coll, level 0]
    (if (and (< level n) (some sequential? ls))
      (recur
        (fold (fn [x res] (apply concat (list res (if (sequential? x) x (list x))))) () ls)
        (inc level)
        )
      ls
      )
    )
  )

; =fnk
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
       (let [k# (->> (split-with (comp keyword? first) (partition 2 args#)) first (flatten-with-depth 1))
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

; =foreach
(defn foreach [f & colls]
  (if (> (count colls) 1)
    (doseq [x (partition 2 (apply interleave colls))] (apply f x))
    (doseq [x (first colls)] (f x))
    )
  )

; =iff
(defn iff
  ([f f2] (fn [x] (if (f x) (f2 x) x)))
  ([x f f2] ((iff f f2) x))
  )

(defn- base->>?* [val sex add-fn]
  (reduce (fn [res x]
            (if (and (list? x) (->> x first (= 'fn*) not))
              (add-fn res x)
              (list x res)
              )
            ) val sex))
; =->*
(defmacro ->* [val & sex]
  (base->>?* val sex (partial insert 1)))
; =->>*
(defmacro ->>* [val & sex]
  (base->>?* val sex #(concat %2 (list %1))))
