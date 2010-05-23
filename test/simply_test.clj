(ns simply-test
  (:use [simply] :reload-all)
  (:use [clojure.test]))

(deftest test-with-implicit-symbol
  (is (with-implicit-symbol _ true))
  (is (with-implicit-symbol _ (= 1 1)))
  (is (with-implicit-symbol _ (+ 1 2) (= 3 _)))
  (is (not (with-implicit-symbol _ (+ 1 2) (= 4 _))))
  (is (with-implicit (+ 1 2) (= 3 %)))
  (is (not (with-implicit (+ 1 2) (= 4 %))))
  (is ((fni [x] (+ 1 x) (= 3 %)) 2))
  (is (not ((fni [x] (+ 1 x) (= 4 %)) 2)))
  (defni _t2 [x] (+ 1 x) (= 3 %))
  (is (_t2 2))
  (is (not (_t2 3)))
  (letfni [(f [x] (inc x) (= % 3))
           (g [x] (= x true))
           ]
    (is (g (f 2)))
    (is (not (g (f 3))))
    )
  )

(deftest test-defnk
  (let [f (fnk [a :b 1 :c 2] (+ a (* b 2) (* c 3)))
        f2 (fnk [a :b 1 :c 2 & more] (- (+ a (* b 2) (* c 3)) (apply + more)))
        ]
    (is (= 9 (f 1)))
    (is (= 11 (f 1 :b 2)))
    (is (= 12 (f 1 :c 3)))
    (is (= 14 (f 1 :c 3 :b 2)))

    (is (= 9 (f2 1)))
    (is (= 11 (f2 1 :b 2)))
    (is (= 12 (f2 1 :c 3)))
    (is (= 14 (f2 1 :c 3 :b 2)))

    (is (= 8 (f2 1 :b 2 3)))
    (is (= 4 (f2 1 :b 2 3 4)))
    (is (= 0 (f2 1 :c 1 :b 1 6)))
    (is (= 0 (f2 0 1 2 3 2)))
    )
  )

(deftest test-p
  (is (p true))
  (is (= 3 (p + 1 2)))
  )

(deftest test-!=
  (is (! false))
  (is (not (! true)))
  (is (! = true false))
  (is (not (! = true true)))
  (is (! = true false true))
  (is (! = 1 2 3))
  )

(deftest test-case
  (is (case 1
        1 true
        :else false
        ))
  (is (not (case 2
             1 true
             :else false
             )))
  (is (case :k
        :k (do false true)
        :else false
        ))
  (is (case "ok"
        "ng" false
        "ok" true
        :else false
        ))
  (let [f (fn [x]
            (case x
              [1 2] true
              3 "ok"
              :else false
              )
            )]
    (is (f 1))
    (is (f 2))
    (is (= "ok" (f 3)))
    (is (not (f 4)))
    )
  )

(deftest test-and-nil?
  (is (and-nil? nil))
  (is (not (and-nil? 1)))
  (is (and-nil? nil nil nil))
  (is (not (and-nil? nil 1 nil)))
  )

(deftest test-or-nil?
  (is (or-nil? nil))
  (is (not (or-nil? 1)))
  (is (or-nil? nil 1 2 3 nil))
  (is (not (or-nil? 1 2 3 4)))
  )

(deftest test-keyword->symbol
  (is (= 'test (keyword->symbol :test)))
  (is (not (= 'test (keyword->symbol :xtest))))
  (is (thrown? java.lang.AssertionError (keyword->symbol "string")))
  )

(deftest test-arithmetic
  (is (= 2 (++ 1)))
  (is (= 1 (-- 2)))
  )

(deftest test-caar-cadr-cddr
  (is (= 1 (caar '((1) (2)))))
  (is (nil? (caar ())))
  (is (thrown? java.lang.IllegalArgumentException (caar '(1))))

  (is (= 2 (cadr '(1 2 3))))
  (is (nil? (cadr ())))
  (is (nil? (cadr '(1))))

  (is (= '(3) (cddr '(1 2 3))))
  (is (empty? (cddr ())))
  (is (empty? (cddr '(1))))
  (is (empty? (cddr '(1 2))))
  )

(deftest test-foreach
  (is (nil? (foreach inc '(1 2 3))))
  (is (nil? (foreach inc '(1 2) '(3 4))))
  (is (thrown? java.lang.AssertionError (foreach inc "hello")))
  (is (thrown? java.lang.AssertionError (foreach inc '(1 2) "hello")))
  (is (thrown? java.lang.AssertionError (foreach inc '(1 2) '(3 4) '5)))
  )

(deftest test-fold
  (is (= 6 (fold + 0 '(1 2 3))))
  (is (not (= '(1 2 3) (fold cons '() '(1 2 3)))))
  (is (= '(1 2 3) (fold cons '() '(3 2 1))))
  (is (thrown? java.lang.AssertionError (fold #(cons %1 %2) '() "neko")))

  (is (= 6 (r-fold + 0 '(1 2 3))))
  (is (= '(1 2 3) (r-fold cons '() '(1 2 3))))
  (is (not (= '(1 2 3) (r-fold cons '() '(3 2 1)))))
  (is (thrown? java.lang.AssertionError (r-fold #(cons %1 %2) '() "neko")))
  )

(deftest test-key-value-seq?
  (is (not (key-value-seq? nil)))
  (is (not (key-value-seq? ())))
  (is (key-value-seq? '(:a 1 :b 2 :c 3)))
  (is (not (key-value-seq? '(:a 1 2 :c 3))))
  (is (not (key-value-seq? '(:a 1 2 3 :c 4))))
  )

(deftest test-i
  (is (= 10 (i "10")))
  (is (= 10 (i '10)))
  (is (= 10 (i :10)))
  (is (= 11 (+ 1 (i '10))))
  )

(deftest test-join
  (is (= "abc" (empty-join '("a" "b" "c"))))
  (is (= "a\nb\nc") (newline-join '("a" "b" "c")))
  )

(deftest test-mk-str
  (is (= "aa" (make-str 2 "a")))
  (is (= "aa" (make-str 2 \a)))
  (is (= "00" (make-str 2 0)))
  (is (= "a" (make-str 1 'a)))
  (is (thrown? java.lang.AssertionError (make-str 0 "a")))
  (is (thrown? java.lang.AssertionError (make-str -1 "a")))
  )

(deftest test-nd
  (is (= "ca" (nd 2 "a" "c")))
  (is (= "a" (nd 1 "a" "c")))
  (is (= "aa" (nd 2 "aa" "c")))
  (is (= "aaa" (nd 2 "aaa" "c")))
  (is (thrown? java.lang.AssertionError (nd 0 "a" "c")))
  (is (thrown? java.lang.AssertionError (nd -1 "a" "c")))
  (is (= "01" (nd 2 1)))
  (is (= "10" (nd 2 10)))
  (is (= "100" (nd 2 100)))
  )

(deftest test-delete-html-tag
  (is (= "hello" (delete-html-tag "<p>hello</p>")))
  (is (= "hello" (delete-html-tag "<p><a>hello</a></p>")))
  (is (= "hello" (delete-html-tag "<p><a href='index.html'>hello</a></p>")))
  (is (= "hello" (delete-html-tag "<p>h</p><p>e</p><p>l</p><p>l</p><p>o</p>")))
  )

(deftest test-struct
  (defstruct teststruct :a :b :c)
  (let [x (struct teststruct 1 2 3)
        y (ref-struct teststruct 10 20 30)
        ]

    (is (ref? (ref x)))
    (is (ref? y))
    (is (not (ref? 123)))
    (is (not (ref? "hello")))

    (is (map? @y))
    (is (= 10 (:a @y)))
    (is (= 20 (:b @y)))
    (is (= 30 (:c @y)))

    (update-struct y :a 1)
    (is (= 1 (:a @y)))
    (update-struct y :b 2 :c 3 :d 4)
    (is (and (= 2 (:b @y)) (= 3 (:c @y)) (= 4 (:d @y))))
    (is (thrown? java.lang.AssertionError (update-struct "hello" :a 1 2)))
    (is (thrown? java.lang.AssertionError (update-struct y :a 1 2)))
    (is (thrown? java.lang.AssertionError (update-struct y :a 1 2 3)))
    )
  )

(deftest test-match?
  (is (match? #"^h"))
  (is (match? #"^h" "hello"))
  (is (not (match? #"^e" "hello")))
  (is (match? #"^h" "hello" "heiho"))
  (is (not (match? #"^h" "hello" "world")))
  (is (not (match? #"^h" "world" "hello")))
  (is (not (match? #"^h" "neko" "world")))
  )

(deftest test-try-with
  (is (= 1 (try-with 1 2 (+ 1 2))))
  (is (= 2 (try-with 1 2 (/ 1 0))))
  (is (try-with-boolean (+ 1 2 3)))
  (is (not (try-with-boolean (/ 1 0))))
  )


