#lang racket

(require (for-syntax "impl-loader.rkt"))

;;--------------------------------
;; Define an implementation loader
;;--------------------------------

(define-syntax load-tensors
  (λ (x)
    (printf "Tensor implementation (no-overrides): ~s~%" (tensor-implementation))
    #`(begin
        #,(case (tensor-implementation)
            ((lazy) #'(require "lazy/no-overrides.rkt"))
            ((learner) #'(require "learner/no-overrides.rkt"))
            ((flat-tensors) #'(require "flat-tensors/no-overrides.rkt"))
            ((nested-tensors) #'(require "nested-tensors/no-overrides.rkt")))
        #,(case (tensor-implementation)
            ((lazy) #'(provide (all-from-out "lazy/no-overrides.rkt")))
            ((learner) #'(provide (all-from-out "learner/no-overrides.rkt")))
            ((flat-tensors) #'(provide (all-from-out "flat-tensors/no-overrides.rkt")))
            ((nested-tensors) #'(provide (all-from-out "nested-tensors/no-overrides.rkt")))))))

(load-tensors)
