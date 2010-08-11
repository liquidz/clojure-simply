(ns simply.date
  (:use simply.string)
  (:require [clojure.contrib.string :as st])
  (:import [java.util TimeZone Calendar GregorianCalendar])
  )

(def *key-calendar-field*
  {:year Calendar/YEAR
   :month Calendar/MONTH
   :day Calendar/DAY_OF_MONTH
   :hour Calendar/HOUR_OF_DAY
   :minute Calendar/MINUTE
   :second Calendar/SECOND}
  )

; =set-default-timezone
(defn set-default-timezone
  ([timezone]
   (TimeZone/setDefault (TimeZone/getTimeZone timezone))
   )
  ([] (set-default-timezone "Asia/Tokyo"))
  )

; =calendar-format
(defn calendar-format [& args]
  (let [klass (-> args first class)
        [cal more] (if (some #(= klass %) (list Calendar GregorianCalendar))
                     [(first args) (rest args)]
                     [(Calendar/getInstance) args]
                     )
        ]
    (st/join
      ""
      (map #(let [r (if (keyword? %) (.get cal (% *key-calendar-field*)) %)]
              (case %
                :month (nd 2 (inc r))
                (:day :hour :minute :second) (nd 2 r)
                r
                )
              ) more)
      )
    )
  )

; =today
(defn today
  ([] (today "/"))
  ([sep] (calendar-format :year sep :month sep :day))
  )

; =now
(defn now
  ([] (now "/" ":"))
  ([sep] (now sep ":"))
  ([ds ts] (calendar-format :year ds :month ds :day " " :hour ts :minute ts :second))
  )

