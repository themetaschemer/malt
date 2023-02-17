#lang info

(define collection "malt")
(define deps '("base" "rackunit-lib"))
(define pkg-desc "A MAchine Learning Toolkit accompanying The Little Learner: A Straight Line to Deep Learning by Daniel P. Friedman and Anurag Mendhekar")
(define version "0.1.0")
(define compile-omit-paths (list #rx"test\\" #rx"test/"))
(define test-omit-paths (list #rx"test\\" #rx"test/"))
(define pkg-authors '("Anurag Mendhekar" "Daniel P. Friedman"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/malt.scrbl" (multi-page))))
(define license 'MIT)
