#lang racket

(define-for-syntax env
  (λ (v)
    (cond
      ((getenv v) => string->symbol)
      (else #f))))

(define-for-syntax tensor-implementation (or (env "MALT_IMPL") 'flat-tensors))

(define-syntax load-tensors
  (λ (x)
    #`(begin
        #,(case tensor-implementation
            ((flat-tensors) #'(require "flat-tensors/no-duals.rkt"))
            ((nested-tensors) #'(require "nested-tensors/no-duals.rkt")))
        #,(case tensor-implementation
            ((flat-tensors) #'(provide (all-from-out "flat-tensors/no-duals.rkt")))
            ((nested-tensors) #'(provide (all-from-out "nested-tensors/no-duals.rkt")))))))

(load-tensors)
