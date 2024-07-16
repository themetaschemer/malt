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
            ((lazy) #'(require "lazy.rkt"))
            ((learner) #'(require "learner.rkt"))
            ((flat-tensors) #'(require "flat-tensors.rkt"))
            ((nested-tensors) #'(require "nested-tensors.rkt")))
        #,(case (tensor-implementation)
            ((lazy) #'(provide (all-from-out "lazy.rkt")))
            ((learner) #'(provide (all-from-out "learner.rkt")))
            ((flat-tensors) #'(provide (all-from-out "flat-tensors.rkt")))
            ((nested-tensors) #'(provide (all-from-out "nested-tensors.rkt")))))))

(load-tensors)
