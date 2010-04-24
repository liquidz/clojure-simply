(ns simply
  (:require [clojure.contrib.seq-utils :as squ])
  (:require [clojure.contrib.str-utils2 :as su2])
  (:require [clojure.contrib.def :as cd])
  )

(declare
  keyword->symbol ++ -- caar cadr cddr foreach fold r-fold 
  key-value-seq? str-convert-encode to-utf8 to-euc to-sjis
  empty-join newline-join make-str nd
  ref? ref-struct update-struct match?
  )

;; DEF {{{
; =with-implicit-symbol
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

; =defni
(defmacro fni [args & body]
  (if (and (rest body) (map? (first body)))
    `(fn ~args ~(first body) (with-implicit ~@(rest body)))
    `(fn ~args (with-implicit ~@body))
    )
  )
(defmacro defni [name & body] `(def ~name (fni ~@body)))
(defmacro defni- [name & decls]
  (list* `defni (with-meta name (assoc (meta name) :private true)) decls)
  )
(defmacro letfni [fnspecs & body]
  (let [args (map (fn [x]
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

; =defnk
(defmacro fnk [args & body]
  (let [[fixed-args k-args] (split-with symbol? args)
        s-args (map #(if (keyword? %) (keyword->symbol %) %) k-args)
        keywords (filter symbol? s-args)
        default-map (apply hash-map s-args)
        [condition-map & rest-body] (if (and (rest body) (map? (first body))) body (cons () body))
        ]
    `(fn [~@fixed-args & more#]
       ~condition-map
       (let [{:keys [~@keywords] :or ~default-map} (apply hash-map more#)]
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

;; }}}

;; =OUTPUT ------------------------------- {{{
; =p
(defmacro p
  ([v] `(do (println ~v) ~v))
  ([l & more]
   `(let [x# (~l ~@more)]
      (println x#)
      x#
      )
   )
  )
;; }}}

;; =CONDITION ------------------------------- {{{
; =!
(defmacro !
  ([v] `(not ~v))
  ([v & more] `(not (~v ~@more)))
  )

; =case
(defmacro case [base-val & patterns]
  (cons
    'cond
    (fold (fn [[val & more] res]
            (concat
              res
              (list (cond
                      (vector? val) `(or ~@(map #(list '= base-val %) val))
                      (= val :else) val
                      :else `(= ~base-val ~val))
                    (first more))))
          ()
          (partition 2 patterns))
    )
  )

;; }}}

;; =SYMBOL ------------------------------- {{{
; =keyword->symbol
(defn keyword->symbol [k]
  {:pre [(keyword? k)] :post [(symbol? %)]}
  (-> k name symbol)
  )
;; }}}

;; =ARITHMETIC ------------------------------- {{{
; =++
(def ++ inc)
; =--
(def -- dec)
;; }}}

;; =COLLECTION ------------------------------- {{{
; =caar
(defn caar [col] (-> col first first))
; =cadr
(defn cadr [col] (-> col rest first))
; =cddr
(defn cddr [col] (-> col rest rest))
; =foreach
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

; =fold
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

; =r-fold
(defni r-fold [f val seq]
  (fold f val seq)
  (if (seq? %) (reverse %) %)
  )

; =key-value-seq?
(defn key-value-seq? [seq]
  (and
    (seq? seq)
    (not (empty? seq))
    (zero? (rem (count seq) 2))
    (every? keyword? (map first (partition 2 seq)))
    )
  )
;; }}}

;; =STRING ------------------------------- {{{
; =str-convert-encoding
(defn str-convert-encoding [encoding & strs]
  {:pre [(string? encoding)]
   :post [(string? %)]
   }
  (String. (.getBytes (apply str strs) encoding))
  )
; =to-utf8
(def to-utf8 (partial str-convert-encoding "UTF-8"))
; =to-euc
(def to-euc (partial str-convert-encoding "EUC-JP"))
; =to-sjis
(def to-sjis (partial str-convert-encoding "Shift_JIS"))
; =empty-join
(def empty-join (partial su2/join ""))
; =newline-join
(def newline-join (partial su2/join "\n"))

; =make-str
(defn make-str [n s]
  {:pre [(pos? n)]}
  (nth (iterate #(str s %) (str s)) (-- n))
  )
; =nd (n-digit)
(defn nd
  ([n s c]
   {:pre [(string? s) (pos? n) (or (string? c) (char? c))]}
   (let [st (str s), len (count st), cs (str c)]
     (str (if (< len n) (make-str (- n len) c) "") st)
     )
   )
  ([n s] (nd n s "0"))
  )

;; }}}

;; =STRUCT ------------------------------- {{{
; =ref?
(defn ref? [x] (= clojure.lang.Ref (class x)))
; =ref-struct
(defn ref-struct [struct-name & keys]
  (ref (apply struct (cons struct-name keys)))
  )
; =update-struct
(defn update-struct [ref & kv]
  {:pre [(ref? ref)
         (key-value-seq? kv)
         ]
   }
  (dosync
    (ref-set
      ref
      (apply assoc (cons @ref kv))
      )
    )
  ref
  )
;; }}}

;; =REGEXP ------------------------------- {{{
; =match?
(defn match? [re & s] (every? #(not (nil? (re-find re %))) s))
;; }}}
