(ns simply-test
  (:use [simply core list regexp string date more] :reload-all)
  (:use [clojure.test]))

(deftest test-defnk
  (let [f (fnk [a :b 1 :c 2] (+ a (* b 2) (* c 3)))
        f2 (fnk [a :b 1 :c 2 & more] (- (+ a (* b 2) (* c 3)) (apply + more)))
        f3 (fnk [a b] (+ a b))
        f4 (fnk [& c] (apply + c))
        ]
    (are [x y] (= x y)
      9   (f 1)
      11  (f 1 :b 2)
      12  (f 1 :c 3)
      14  (f 1 :c 3 :b 2)

      9   (f2 1)
      11  (f2 1 :b 2)
      12  (f2 1 :c 3)
      14  (f2 1 :c 3 :b 2)

      8   (f2 1 :b 2 3)
      4   (f2 1 :b 2 3 4)
      0   (f2 1 :c 1 :b 1 6)
      0   (f2 0 1 2 3 2)

      3   (f3 1 2)

      6   (f4 1 2 3)
      10  (f4 1 2 3 4)
      )
    )
  )

(deftest test-!=
  (are [x y] (= x y)
    true  (! false)
    false (! true)
    true  (! = true false)
    false (! = true true)
    true  (! = true false true)
    true  (! = 1 2 3)
    )
  )

(deftest test-foreach
  (are [x] (nil? x)
    (foreach inc nil)
    (foreach inc '(1 2 3))
    (foreach + '(1 2) '(3 4))
    (foreach inc [1 2 3])
    (foreach (fn [[k v]] v) {:a 1 :b 2})
    (foreach (fn [[k v]] v) ())
    )
  )

(deftest test-delete-duplicates
  (are [x y] (= x (count (delete-duplicates y)))
    0 ()
    0 []
    4 [1 2 3 2 1 3 4]
    4 '(a b a c b d)
    )

  (let [sample [{:a 1 :b 2} {:a 2 :b 3} {:a 1 :b 3} {:a 3 :b 4} {:a 2 :b 4} {:a 4 :b 5} {:a 4 :b 5}]
        a-ls (delete-duplicates :a sample)
        b-ls (delete-duplicates :b sample)
        ]
    (are [x y] (= x y)
      4  (count a-ls)
      4  (count b-ls)
      10 (reduce #(+ (:a %2) %1) 0 a-ls)
      14 (reduce #(+ (:b %2) %1) 0 b-ls)
      )
    )
  )

#_(deftest test-integer
  (are [x y] (= x (integer y))
    10 "10"
    10 '10
    10 :10
    )
  )

(deftest test-delete-html-tag
  (are [x y] (= x (delete-html-tag y))
    ""      "<></>"
    "hello" "<p>hello</p>"
    "hello" "<p><a>hello</a></p>"
    "hello" "<p><a href='index.html'>hello</a></p>"
    "hello" "<p>h</p><p>e</p><p>l</p><p>l</p><p>o</p>"
    )
  )

(deftest test-str-compare
  (are [x y] (= x y)
    true  (str-compare zero? "a" "a")
    false (str-compare zero? "a" "b")
    true  (str< "a" "b")
    false (str< "a" "a")
    false (str< "b" "a")
    true  (str> "b" "a")
    false (str> "b" "b")
    false (str> "a" "b")
    )
  )

(deftest test-escape
  (are [x y] (= x (escape y))
    ""             ""
    "test"         "<s>test</s>"
    "test"         "t>e\"s't<"
    "test"         ">>>>>>>>>>>>te<><><><>st<<<<<<<<<<<<<"
    "alert(test);" "\"><script>alert('test');</script>"
    ""             (list 1 2 3)
    )
  )

(deftest test-match?
  (are [x y] (= x (apply match? y))
    true  '(#"^h")
    true  '(#"^h" "hello")
    false '(#"^e" "hello")
    true  '(#"^h" "hello" "heiho")
    false '(#"^h" "hello" "world")
    false '(#"^h" "world" "hello")
    false '(#"^h" "neko" "world")
    )
  )

#_(deftest test-try-with
  (are [x y] (= x y)
    1     (try-with 1 2 (+ 1 2))
    2     (try-with 1 2 (/ 1 0))
    true  (try-success? (+ 1 2 3))
    false (try-success? (/ 1 0))
    )
  )


(deftest test-calendar-format
  (let [c (java.util.Calendar/getInstance)]
    (.set c 2010 (dec 1) 2 3 4 5)
    (are [x y] (= x y)
      "20100102" (calendar-format c :year :month :day)
      "030405" (calendar-format c :hour :minute :second)
      "2010a01b02" (calendar-format c :year "a" :month "b" :day)
      "2010a01b02c"(calendar-format c :year "a" :month "b" :day "c")
      )

    (.set c 2010 (dec 10) 12 13 14 15)
    (are [x y] (= x y)
      "20101012" (calendar-format c :year :month :day)
      "131415" (calendar-format c :hour :minute :second)
      )
    (is (calendar-format :year :month :day))
    (is (calendar-format "/" :year :month :day))
    )
  )

(deftest test-today
  (is (today))
  (is (today "-"))
  )

(deftest test-now
  (is (now))
  (is (now "-"))
  (is (now "-" ","))
  )

(deftest test-set-default-time-zone
  (is (not (set-default-timezone)))
  (is (not (set-default-timezone "Asia/Tokyo")))
  )

(deftest test-++
  (are [x y] (= x y)
    10 (++ 1 2 3 4)
    "helloworld" (++ "hello" "world")
    '(1 2 3 4) (++ '(1 2) '(3) '(4))
    [1 2 3 4] (++ [1] [2 3] [4])
    '("a" "b" "c") (->> (++ {'a 1} {'b 2 'c 3}) keys (map str) (sort str<))
    4 ((++ inc inc inc) 1)
    )
  )

