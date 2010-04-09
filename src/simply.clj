(ns simply
  (:require [clojure.contrib.seq-utils :as squ])
  (:require [clojure.contrib.str-utils2 :as su2])
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

(defmacro defi [name & body]
  `(def ~name (with-implicit ~@body))
  )

(defmacro fni [args & body]
  `(fn ~args (with-implicit ~@body))
  )

(defmacro defni [name args & body]
  `(def ~name (fni ~args ~@body))
  )

(defmacro defni- [name & decls]
  (list* `defni (with-meta name (assoc (meta name) :private true)) decls)
  )

(defn- collect-keywords [from to]
  {:pre [(and (zero? (rem (count from) 2)) (zero? (rem (count to) 2)))]}
  (let [from-pair (partition 2 from)
        to-pair (partition 2 to)
        ]
    (map (fni [x]
           (squ/find-first #(= (first x) (first %)) from-pair)
           (if (nil? %) x %)
           )
         to-pair)
    )
  )

(defmacro let-keywords [base key-map & body]
  (let [base-ls (map #(if (keyword? %) (keyword->symbol %) %) base)
        args (squ/flatten (collect-keywords base-ls key-map))
        ]
    `(let [~@args] ~@body)
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
(defn foreach [f & seq-exprs]
  {:pre [(every? seq? seq-exprs)]}
  (doseq [seq seq-exprs]
    (doseq [x seq] (f x))
    )
  )

(defn fold [f val seq]
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
