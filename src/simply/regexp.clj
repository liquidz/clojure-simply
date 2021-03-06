(ns simply.regexp)

; =regexp?
(defn regexp? [obj] (= java.util.regex.Pattern (class obj)))

; =match?
(defn match? [re & s] (not (some #(nil? (re-seq re %)) s)))

