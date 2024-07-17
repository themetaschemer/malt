#lang racket

;;—————————————————–—————————————————–—————————————————–
;; Equality checks for mostly for testing.
;;—————————————————–—————————————————–—————————————————–

(require "0-vectors.ss")
(require "1-flats.ss")
(require rackunit)

;;—————————————————–—————————————————–—————————————————–
;; These parameters can be overriden to account for
;; different type of numbers used inside tensors.
;;—————————————————–—————————————————–—————————————————–

(define tolerance (make-parameter 0.0001))

(define equal-within-tolerance?
  (make-parameter
   (λ (actual expected)
     (< (abs (- actual expected)) (tolerance)))))

;;—————————————————–—————————————————–—————————————————–
;; These are representation specific, but part of the
;; exported interface of the module
;;—————————————————–—————————————————–—————————————————–

(define tensor-equal?
  (λ (actual expected)
    (or (equal? actual expected)
        (and (real? actual)
             (real? expected)
             ((equal-within-tolerance?) actual expected))
        (and (flat? actual)
             (flat? expected)
             (equal? (flat-shape actual)
                     (flat-shape expected))
             (equal-elements? actual expected)))))

(define (equal-elements? actual expected)
  (let ((actual-offset (flat-offset actual))
        (expected-offset (flat-offset expected))
        (actual-size (flat-size actual))
        (expected-size (flat-size expected))
        (actual-store (flat-store actual))
        (expected-store (flat-store expected)))
    (and (equal? actual-size expected-size)
         (call/cc (λ (return)
                    (for/fold ([check #t])
                              ([i-actual (in-range actual-offset
                                                   (+ actual-offset
                                                      actual-size))]
                               [i-expected (in-range expected-offset
                                                     (+ expected-offset
                                                        expected-size))])
                      (cond
                        (((equal-within-tolerance?)
                          (vref actual-store i-actual)
                          (vref expected-store i-expected)) check)
                        (else (return #f)))))))))

(define-check (check-tensor-equal? actual expected)
  (unless (tensor-equal? actual expected)
    (fail-check (format "Tensors failed to match.~%actual:~%~s~%expected:~s~%~%actual store:~%~s~%expected store:~s~%"
                        actual expected
                        (with-output-to-string (λ () (print-vec (flat-store actual))))
                        (with-output-to-string (λ () (print-vec (flat-store expected))))))))

(include "test/test-A-equality.rkt")

(provide tolerance equal-within-tolerance? tensor-equal? check-tensor-equal? equal-elements?)
