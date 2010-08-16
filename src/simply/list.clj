(ns simply.list
  (:require [clojure.contrib.seq :as se])
  )

; =caar
(defn caar [col] (-> col first first))
; =cadr
(defn cadr [col] (-> col rest first))
; =cddr
(defn cddr [col] (-> col rest rest))
; =foreach
(defn foreach [f & seq-exprs]
  {:pre [(every? #(or (nil? %) (seq? %) (vector? %) (map? %)) seq-exprs)]}
  (doseq [seq seq-exprs]
    (doseq [x seq] (f x))
    )
  )

; =fold
(defn fold [f val col]
  {:pre [(or (seq? col) (vector? col) (map? col))]}
  (loop [res val, ls col]
    (if (empty? ls)
      res
      (recur (f (first ls) res) (rest ls))
      )
    )
  )

; =r-fold
(defn r-fold [f val seq]
  (let [res (fold f val seq)]
    (if (seq? res) (reverse res) res)
    )
  )

; =group
(defn group
  ([col] (group (fn [x] x) col))
  ([get-key-f col]
   (fold
     (fn [x res]
       (let [tmp (get-key-f x)
             key (keyword (if (number? tmp) (str tmp) tmp))]
         (assoc res key (if (nil? (key res)) (list x) (cons x (key res))))
         )
       ) {} col)
   )
  )

; =delete-duplicates
(defn delete-duplicates
  ([f col]
   (loop [ls col res ()]
     (if (empty? ls)
       (reverse res)
       (let [val (first ls)]
         (recur (rest ls)
                (if (nil? (se/find-first #(= (f %) (f val)) res))
                  (cons val res) res))
         )
       )
     )
   )
  ([col] (delete-duplicates identity col))
  )

; =deep-map
(defn deep-map [f coll]
  {:pre [(fn? f) (coll? coll)]}
  (let [res (map #(if (coll? %) (deep-map f %) (f %)) coll)]
    (if (vector? coll) (vec res) res)
    )
  )

; deep-replace
(defn deep-replace [smap coll]
  {:pre [(map? smap) (coll? coll)]}
  (deep-map #(let [x (get smap %)] (if (nil? x) % x)) coll)
  )

