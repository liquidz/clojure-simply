(ns simply.string
  (:require [clojure.contrib.string :as st])
  (:import [java.net URLEncoder])
  )

(defn str-convert-encoding [encoding & strs]
  {:pre [(string? encoding)]
   :post [(string? %)]
   }
  (String. (.getBytes (apply str strs) encoding))
  )
; =to-utf8
(def to-utf8 (partial str-convert-encoding "UTF-8"))
(def utf8 to-utf8)
; =to-euc
(def to-euc (partial str-convert-encoding "EUC-JP"))
(def euc to-euc)
; =to-sjis
(def to-sjis (partial str-convert-encoding "Shift_JIS"))
(def sjis to-sjis)

; =delete-html-tag
(def delete-html-tag (partial st/replace-re #"<.+?>" ""))

; =escape
(defn escape [s]
  (if (and (string? s) (not (st/blank? s)))
    (->> s delete-html-tag (st/replace-re #"[\"'<>]" ""))
    ""
    )
  )

; =starts-with?
(defn starts-with? [s s2] (.startsWith s s2))

; =url-encode
(defn url-encode [s & {:keys [encode] :or {encode "UTF-8"}}]
  (URLEncoder/encode s encode)
  )

; =str-compare
(defn str-compare [f s1 s2] (f (.compareTo s1 s2)))
(def str> (partial str-compare pos?))
(def str< (partial str-compare neg?))

; =string->regexp
(defn string->regexp [& s] (java.util.regex.Pattern/compile (apply str s)))

