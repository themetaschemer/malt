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
            ((lazy) #'(require "lazy/no-duals.rkt"))
            ((learner) #'(require "learner/no-duals.rkt"))
            ((flat-tensors) #'(require "flat-tensors/no-duals.rkt"))
            ((nested-tensors) #'(require "nested-tensors/no-duals.rkt")))
        #,(case (tensor-implementation)
            ((lazy) #'(provide (all-from-out "lazy/no-duals.rkt")))
            ((learner) #'(provide (all-from-out "learner/no-duals.rkt")))
            ((flat-tensors) #'(provide (all-from-out "flat-tensors/no-duals.rkt")))
            ((nested-tensors) #'(provide (all-from-out "nested-tensors/no-duals.rkt")))))))

(load-tensors)
