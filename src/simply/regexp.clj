(ns simply.regexp)

(defn match? [re & s] (every? #(not (nil? (re-find re %))) s))
