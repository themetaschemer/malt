#lang racket

;;—————————————————–—————————————————–—————————————————–
;; Equality checks for mostly for testing.
;;—————————————————–—————————————————–—————————————————–

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
    (cond
      ((and (number? actual) (number? expected))
       ((equal-within-tolerance?) actual expected))
      ((and (vector? actual)
            (vector? expected))
       (equal-elements? actual expected))
      (else #f))))

(define (equal-elements? actual expected)
  (let ((actual-size (vector-length actual))
        (expected-size (vector-length expected)))
    (and (equal? actual-size expected-size)
         (call/cc (λ (return)
                    (for/fold ([check #t]) ([i (in-range 0 actual-size)])
                      (cond
                        ((tensor-equal? (vector-ref actual i)
                                        (vector-ref expected i))
                         check)
                        (else (return #f)))))))))

(define-binary-check (check-tensor-equal? tensor-equal? actual expected))

(include "test/test-A-equality.rkt")

(provide tolerance equal-within-tolerance? tensor-equal? check-tensor-equal?)
