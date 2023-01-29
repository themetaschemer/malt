#lang racket

(require (for-syntax "impl-loader.rkt"))

;;--------------------------------
;; Define an implementation loader
;;--------------------------------

(define-syntax load-tensors
  (λ (x)
    (printf "Tensor implementation (no-duals): ~s~%" (tensor-implementation))
    #`(begin
        #,(case (tensor-implementation)
            ((learner) #'(require "learner/no-duals.rkt"))
            ((flat-tensors) #'(require "flat-tensors/no-duals.rkt"))
            ((nested-tensors) #'(require "nested-tensors/no-duals.rkt")))
        #,(case (tensor-implementation)
            ((learner) #'(provide (all-from-out "learner/no-duals.rkt")))
            ((flat-tensors) #'(provide (all-from-out "flat-tensors/no-duals.rkt")))
            ((nested-tensors) #'(provide (all-from-out "nested-tensors/no-duals.rkt")))))))

(load-tensors)
