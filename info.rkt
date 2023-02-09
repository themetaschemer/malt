#lang info

(define collection "malt")
(define deps '("base" "rackunit-lib"))
(define pkg-desc "A MAchine Learning Toolkit")
(define version "0.1")
(define compile-omit-paths (list #rx"test/"))
(define test-omit-paths (list #rx"test/"))
(define pkg-authors '("Anurag Mendhekar" "Daniel P. Friedman"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("malt.scrbl" (multi-page))))
