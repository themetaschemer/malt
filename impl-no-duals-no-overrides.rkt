#lang racket

(require (for-syntax "impl-loader.rkt"))

;;--------------------------------
;; Define an implementation loader
;;--------------------------------

(define-syntax load-tensors
  (λ (x)
    (printf "Tensor implementation (no-duals, no-overrides): ~s~%" (tensor-implementation))
    #`(begin
        #,(case (tensor-implementation)
            ((lazy) #'(require "lazy/no-duals-no-overrides.rkt"))
            ((learner) #'(require "learner/no-duals-no-overrides.rkt"))
            ((flat-tensors) #'(require "flat-tensors/no-duals-no-overrides.rkt"))
            ((nested-tensors) #'(require "nested-tensors/no-duals-no-overrides.rkt")))
        #,(case (tensor-implementation)
            ((lazy) #'(provide (all-from-out "lazy/no-duals-no-overrides.rkt")))
            ((learner) #'(provide (all-from-out "learner/no-duals-no-overrides.rkt")))
            ((flat-tensors) #'(provide (all-from-out "flat-tensors/no-duals-no-overrides.rkt")))
            ((nested-tensors) #'(provide (all-from-out "nested-tensors/no-duals-no-overrides.rkt")))))))

(load-tensors)
