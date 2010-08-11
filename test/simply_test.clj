(ns simply-test
  (:use [simply core def integer list ref regexp string] :reload-all)
  (:use [clojure.test]))

(deftest test-defnk
  (let [f (fnk [a :b 1 :c 2] (+ a (* b 2) (* c 3)))
        f2 (fnk [a :b 1 :c 2 & more] (- (+ a (* b 2) (* c 3)) (apply + more)))
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
      )
    )
  )

(deftest test-pp
  (is (pp true))
  (is (= 3 (pp + 1 2)))
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
    (foreach inc '(1 2) '(3 4))
    (foreach inc [1 2 3])
    (foreach (fn [[k v]] v) {:a 1 :b 2})
    (foreach (fn [[k v]] v) ())
    )
  (is (thrown? java.lang.AssertionError (foreach inc "hello")))
  (is (thrown? java.lang.AssertionError (foreach inc '(1 2) "hello")))
  (is (thrown? java.lang.AssertionError (foreach inc '(1 2) '(3 4) '5)))
  )

(deftest test-fold
  (are [x y] (= x y)
    6         (fold + 0 '(1 2 3))
    '(1 2 3)  (fold cons '() '(3 2 1))

    6         (fold + 0 [1 2 3])
    3         (fold (fn [[m n] res] (+ n res)) 0 {:a 1 :b 2})

    6         (r-fold + 0 '(1 2 3))
    '(1 2 3)  (r-fold cons '() '(1 2 3))
    )

  (is (thrown? java.lang.AssertionError (fold #(cons %1 %2) '() "neko")))
  (is (thrown? java.lang.AssertionError (r-fold #(cons %1 %2) '() "neko")))
  )

(deftest test-group
  (let [res (group '(a b a b a))]
    (are [x y] (= x (count y))
      3 (:a res)
      2 (:b res)
      )
    )

  (let [sample '((a 1) (b 1) (a 2) (c 2) (b 3))
        fres (group first sample)
        sres (group second sample)
        ]
    (are [x y] (= x (count y))
      2 (:a fres)
      2 (:b fres)
      1 (:c fres)

      2 (:1 sres)
      2 (:2 sres)
      1 (:3 sres)
      )
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
      10 (fold #(+ (:a %1) %2) 0 a-ls)
      14 (fold #(+ (:b %1) %2) 0 b-ls)
      )
    )
  )

(deftest test-integer
  (are [x y] (= x (integer y))
    10 "10"
    10 '10
    10 :10
    )
  )

(deftest test-nd
  (are [x y] (= x (apply nd y))
    "ca"  '(2 "a" "c")
    "a"   '(1 "a" "c")
    "aa"  '(2 "aa" "c")
    "aaa" '(2 "aaa" "c")
    "01"  '(2 1)
    "10"  '(2 10)
    "100" '(2 100)
    )
  (is (thrown? java.lang.AssertionError (nd 0 "a" "c")))
  (is (thrown? java.lang.AssertionError (nd -1 "a" "c")))
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

(deftest test-struct
  (defstruct teststruct :a :b :c)
  (let [m (struct teststruct 1 2 3)
        n (ref-struct teststruct 10 20 30)
        ]

    (are [x y] (= x (ref? y))
      true  (ref m)
      true  n
      false 123
      false "hello"
      )

    (are [x y] (= x y)
      true (map? @n)
      10   (:a @n)
      20   (:b @n)
      30   (:c @n)
      )

    (update-struct n :a 1)
    (is (= 1 (:a @n)))
    (update-struct n :b 2 :c 3 :d 4)
    (is (and (= 2 (:b @n)) (= 3 (:c @n)) (= 4 (:d @n))))
    (is (thrown? java.lang.AssertionError (update-struct "hello" :a 1 2)))
    (is (thrown? java.lang.AssertionError (update-struct n :a 1 2)))
    (is (thrown? java.lang.AssertionError (update-struct n :a 1 2 3)))
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

(deftest test-try-with
  (are [x y] (= x y)
    1     (try-with 1 2 (+ 1 2))
    2     (try-with 1 2 (/ 1 0))
    true  (try-success? (+ 1 2 3))
    false (try-success? (/ 1 0))
    )
  )


