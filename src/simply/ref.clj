(ns simply.ref
  (:use [simply.integer :only [dividable?]])
  )

; =ref?
(defn ref? [x] (= clojure.lang.Ref (class x)))
; =ref-struct
(defn ref-struct [struct-name & keys]
  (ref (apply struct (cons struct-name keys)))
  )
; =update-struct
(defn update-struct [ref & kv]
  {:pre [(ref? ref)
         (dividable? (count kv) 2)
         (every? keyword? (map first (partition 2 kv)))
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
