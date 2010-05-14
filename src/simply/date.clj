(ns simply.date
  (:use simply)
  (:import [java.util Calendar GregorianCalendar])
  )

(def *key-calendar-field*
  {:year Calendar/YEAR
   :month Calendar/MONTH
   :day Calendar/DAY_OF_MONTH
   :hour Calendar/HOUR_OF_DAY
   :minute Calendar/MINUTE
   :second Calendar/SECOND}
  )

; =calendar-format
(defn calendar-format [& args]
  (let [klass (-> args first class)
        [cal more] (if (some #(= klass %) (list Calendar GregorianCalendar))
                     [(first args) (rest args)]
                     [(Calendar/getInstance) args]
                     )]
    (empty-join
      (map #(let [r (if (keyword? %) (.get cal (% *key-calendar-field*)) %)]
              (case %
                :month (nd 2 (inc r))
                [:day :hour :minute :second] (nd 2 r)
                :else r
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

