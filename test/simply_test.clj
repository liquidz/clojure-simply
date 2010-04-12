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
  (defi _t (+ 1 2) (= 3 %))
  (defi _t! (+ 1 2) (= 4 %))
  (is _t)
  (is (not _t!))
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

(deftest test-p
  (is (p true))
  (is (= 3 (p + 1 2)))
  )

(deftest test-!=
  (is (!= true false))
  (is (not (!= true true)))
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



