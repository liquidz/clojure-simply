(ns simply-test
  (:use [simply] :reload-all)
  (:use [clojure.test]))

(deftest test-!=
         (is (!= true false))
         (is (not (!= true true)))
         )

(deftest test-foreach
         (is (not (foreach inc '(1 2 3))))
         )

(deftest test-fold
         (is (= 6 (fold #(+ %1 %2) 0 '(1 2 3))))
         (is (= '(1 2 3) (fold #(cons %1 %2) '() '(3 2 1))))
         )



