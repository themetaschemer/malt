#lang racket

(require "../base.rkt")

(define block
  (λ (fn shape-list)
    (list fn shape-list)))

(define block-fn
  (λ (b)
    (ref b 0)))

(define block-ls
  (λ (b)
    (ref b 1)))

(define compose-block-fns
  (λ (fa fb j)
    (λ (t)
      (λ (theta)
        ((fb
           ((fa t) theta))
         (refr theta j))))))

(define stack2
  (λ (ba bb)
    (block
     (compose-block-fns (block-fn ba) (block-fn bb) (len (block-ls ba)))
     (append (block-ls ba) (block-ls bb)))))

(define stack-blocks
  (λ (bls)
    (stacked-blocks (refr bls 1) (ref bls 0))))

(define stacked-blocks
  (λ (rbls b)
    (cond
      ((null? rbls) b)
      (else
       (stacked-blocks (refr rbls 1)
         (stack2 b (ref rbls 0)))))))

(include "test/test-N-blocks.rkt")

(provide block stack-blocks)
