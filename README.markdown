# simply
write clojure code more simply

## Installation

    (defproject your-project "0.0.1-SNAPSHOT"
	  :description ""
	  :dependencies [[org.clojars.liquidz/simply "0.0.1"]]
	  )

## Usage
    (ns your-namespace
	  (:use simply)
	  )

	; simply print
	(p + 1 2 3) ; => (println (+ 1 2 3))

	; !=
	(!= true false) ; => true

	; foreach
	(foreach println '(1 2 3))

	; fold
	(fold #(%1 %2) () '(1 2 3)) ; reverse

	; charconv
	(to-utf8 "...")
	(to-euc "...")
	(to-sjis "...")

