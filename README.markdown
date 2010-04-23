# simply
write clojure code more simply

## Installation

    (defproject your-project "0.0.1-SNAPSHOT"
	  :description ""
	  :dependencies [[org.clojars.liquidz/simply "0.0.2"]]
	  )

## Usage
    (ns your-namespace
	  (:use simply)
	  )

	; implicit-symbol
	(with-implicit
	  (+ 1 2)
	  (= 3 %) ; % = (+ 1 2) = 3
	  (if % "ok" "ng") ; => "ok"
	  )

	(defni implicit-fun [x]
	  (+ 1 x)
	  (= 3 %)
	  )
	(implicit-function 2) ; => true

	; simply print
	(p + 1 2 3) ; => (println (+ 1 2 3))

	; !=
	(!= true false) ; => true

	; foreach
	(foreach println '(1 2 3))

	; fold
	(fold #(cons %1 %2) () '(1 2 3)) ; reverse

	; r-fold
	(r-fold #(cons %1 %2) () '(1 2 3)) ; => (reverse (fold #(cons %1 %2) () '(1 2 3)))

	; key-value-seq?
	(key-value-seq? '(:a 1 :b 2)) ; => true
	(key-value-seq? '(:a 1 2)) ; => false

	; charconv
	(to-utf8 "...")
	(to-euc "...")
	(to-sjis "...")

	; struct
	(ref? (ref {:a 1 :b 2})) ; => true

	(defstruct sample :a :b)
	(ref-struct sample 1 2) ; => (ref (struct sample 1 2))

	(update-struct (ref-struct sample 1 2)
	  :a 10
	  :b 20
	  ) ; => {:a 10 :b 20}


