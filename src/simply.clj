(ns simply
  (:require [clojure.contrib.seq-utils :as squ])
  (:require [clojure.contrib.str-utils2 :as su2])
  (:require [clojure.contrib.def :as cd])
  )

(declare
  != keyword->symbol ++ -- foreach fold r-fold 
  str-convert-encode to-utf8 to-euc to-sjis
  )

;; DEF {{{
(defmacro with-implicit-symbol [s & body]
  (if (> (count body) 1)
    (let [[fst & more] body ]
      (list 'let `[~s ~fst]
            `(with-implicit-symbol ~s ~@more)
            )
      )
    (first body)
    )
  )
(defmacro with-implicit [& body]
  `(with-implicit-symbol ~(quote %) ~@body)
  )

(defmacro defi [name & body] `(def ~name (with-implicit ~@body)))
(defmacro fni [args & body]
  (if (and (rest body) (map? (first body)))
    `(fn ~args ~(first body) (with-implicit ~@(rest body)))
    `(fn ~args (with-implicit ~@body))
    )
  )
(defmacro defni [name args & body] `(def ~name (fni ~args ~@body)))
(defmacro defni- [name & decls]
  (list* `defni (with-meta name (assoc (meta name) :private true)) decls)
  )
(defmacro letfni [fnspecs & body]
  (let [args (map
               (fn [x]
                 (let [[name arg & f-body] x]
                   `(~name ~arg (with-implicit ~@f-body))
                   )
                 )
               fnspecs
               )
        ]
    `(letfn [~@args] ~@body)
    )
  )

; copy of clojure.contrib.def/defnk
(defmacro fnk [args & body]
  (let [[fixed-args k-args] (split-with symbol? args)
        s-args (map #(if (keyword? %) (keyword->symbol %) %) k-args)
        keywords (filter symbol? s-args)
        default-map (apply has-map s-args)
        ]
    `(fn [~@fixed-args & more#]
       (let [{:keys [~@keywords] :or ~default-map} (apply hash-map more#)]
         ~@body
         )
       )
    )
  )

;; }}}

;; =OUTPUT ------------------------------- {{{
(defmacro p
  ([v] `(do (println ~v) ~v))
  ([l & args]
   `(let [x# (~l ~@args)]
      (println x#)
      x#
      )
   )
  )
;; }}}

;; =CONDITIONS ------------------------------- {{{
(defn !=
  ([x] true)
  ([x y] (not (= x y)))
  ([x y & more] (and (= x y) (apply = more)))
  )
;; }}}

;; =SYMBOL ------------------------------- {{{
(defn keyword->symbol [k]
  {:pre [(keyword? k)] :post [(symbol? %)]}
  (symbol (su2/drop (str k) 1))
  )
;; }}}

;; =ARITHMETIC ------------------------------- {{{
(def ++ inc)
(def -- dec)
;; }}}

;; =SEQUENCE ------------------------------- {{{
(defn foreach
  "(foreach function sequences*)

  ex. (foreach println '(1 2 3) '(4 5 6)) ; => 123456nil
  "
  [f & seq-exprs]
  {:pre [(every? seq? seq-exprs)]}
  (doseq [seq seq-exprs]
    (doseq [x seq] (f x))
    )
  )

(defn fold
  "(fold (fn [item result] exprs*) initial-value sequence)

   ex. (fold cons () '(1 2 3)) ; => (3 2 1)
  "
  [f val seq]
  {:pre [(seq? seq)]}
  (loop [res val, ls seq]
    (if (empty? ls)
      res
      (recur (f (first ls) res) (rest ls))
      )
    )
  )

(defni r-fold [f val seq]
  (fold f val seq)
  (if (seq? %) (reverse %) %)
  )
;; }}}

;; =STRING ------------------------------- {{{
(defn str-convert-encode [encoding & strs]
  {:pre [(string? encoding)]
   :post [(string? %)]
   }
  (String. (.getBytes (apply str strs) encoding))
  )
(def to-utf8 (partial str-convert-encode "UTF-8"))
(def to-euc (partial str-convert-encode "EUC-JP"))
(def to-sjis (partial str-convert-encode "Shift_JIS"))
;; }}}
