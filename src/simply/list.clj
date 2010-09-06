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

; deep-replace
(defn deep-replace [smap coll]
  {:pre [(map? smap) (coll? coll)]}
  (deep-map #(let [x (get smap %)] (if (nil? x) % x)) coll)
  )

