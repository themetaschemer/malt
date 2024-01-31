#lang racket

(require (for-syntax "impl-loader.rkt"))

;;--------------------------------
;; Define an implementation loader
;;--------------------------------

(define-syntax load-tensors
  (λ (x)
    (printf "Tensor implementation: ~s~%" (tensor-implementation))
    #`(begin
        #,(case (tensor-implementation)
            ((learner) #'(require "learner.rkt"))
            ((flat-tensors) #'(require "flat-tensors.rkt"))
            ((uniform-tensors) #'(require "uniform-tensors.rkt"))
            ((nested-tensors) #'(require "nested-tensors.rkt")))
        #,(case (tensor-implementation)
            ((learner) #'(provide (all-from-out "learner.rkt")))
            ((flat-tensors) #'(provide (all-from-out "flat-tensors.rkt")))
            ((uniform-tensors) #'(provide (all-from-out "uniform-tensors.rkt")))
            ((nested-tensors) #'(provide (all-from-out "nested-tensors.rkt")))))))

(load-tensors)
