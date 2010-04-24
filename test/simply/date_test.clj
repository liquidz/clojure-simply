(ns simply.date-test
  (:use [simply] :reload-all)
  (:use [simply.date] :reload-all)
  (:use [clojure.test]))

(deftest test-calendar-format
  (let [c (java.util.Calendar/getInstance)]
    (.set c 2010 (dec 1) 2 3 4 5)
    (is (= "20100102" (calendar-format c :year :month :day)))
    (is (= "030405") (calendar-format c :hour :minute :second))
    (is (= "2010a01b02"(calendar-format c :year "a" :month "b" :day)))
    (is (= "2010a01b02c"(calendar-format c :year "a" :month "b" :day "c")))

    (.set c 2010 (dec 10) 12 13 14 15)
    (is (= "20101012" (calendar-format c :year :month :day)))
    (is (= "131415" (calendar-format c :hour :minute :second)))

    (is (calendar-format :year :month :day))
    (is (calendar-format "/" :year :month :day))
    )
  )
