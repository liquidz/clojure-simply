(ns simply.list
  (:require [clojure.contrib.seq :as se])
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

; =deep-replace
(defn deep-replace [smap coll]
  {:pre [(map? smap) (coll? coll)]}
  (deep-map #(let [x (get smap %)] (if (nil? x) % x)) coll)
  )

; =find-index
(defn find-index
  ([conv pred coll]
   (->> coll se/indexed conv (se/find-first (comp pred second)) first))
  ([pred coll] (find-index identity pred coll))
  )
; =find-first-index
(def find-first-index (partial find-index identity))
; =find-last-index
(def find-last-index (partial find-index reverse))
