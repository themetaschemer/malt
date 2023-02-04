#lang racket

(require "base.rkt")
(require "malted.rkt")

(provide

 ;; Lists
 len ref refr

 ;; Tensor basics
 tref tlen list->tensor tensor build-tensor
 scalar? tensor? rank shape reshape trefs

 ;; extended operations
 ext1-ρ ext2-ρ ext1-∇ ext2-∇
 ext1 ext2 prim1 prim2

 ;; Duals & AD
 dual dual? ρ κ ∇ ∇¹ (rename-out (∇ gradient-of))

 ;; Differentiable numerical operators
  + - * / rectify
  exp log expt sqrt sqr
  sum abs *-2-1 argmax
  max sum-cols correlate
  dot-product-2-1 dot-product


 ;; Non-differentiable numerical operators
 +-ρ --ρ *-ρ /-ρ rectify-ρ
 exp-ρ log-ρ expt-ρ sqrt-ρ sqr-ρ
 sum-ρ abs-ρ *-2-1-ρ argmax-ρ
 max-ρ sum-cols-ρ correlate-ρ

 ;; Differentiable scalar base-rank operators
 +-0-0 --0-0 *-0-0 /-0-0 expt-0-0
 exp-0 log-0 abs-0 rectify-0

 sum-1 argmax-1 max-1

 ;; Comparators
 =-0-0 <-0-0 <=-0-0 >-0-0 >=-0-0
 (rename-out (=-0-0 =)
             (<-0-0 <)
             (<=-0-0 <=)
             (>-0-0 >)
             (>=-0-0 >=))

 ;; Tensorized comparators
 =-1 <-1 >-1 <=-1 >=-1 !=-1

 ;; Random Number Generation
 random-normal random-standard-normal
 random-tensor

 ;; Logging utilities
 record
 with-recording
 start-logging
 log-malt-fatal
 log-malt-error
 log-malt-warning
 log-malt-info
 log-malt-debug

 ;; Hyperparameters and grid search
 with-hyper with-hypers
 declare-hyper declare-hypers grid-search

 ;; Gradient descent
 revise
 gradient-descent
 samples sampling-obj
 revs alpha batch-size


 naked-gradient-descent

 mu
 velocity-gradient-descent

 beta smooth epsilon
 rms-gradient-descent
 adam-gradient-descent

 ;; Layer functions
 line quad linear-1-1 linear plane softmax
 relu k-relu  recu corr k-recu
 avg-cols signal-avg

 ;; Loss functions
 l2-loss cross-entropy-loss kl-loss

 ;; Blocks and block composition
 block block-fn block-ls stack-blocks

 ;; Initialization of theta
 init-theta init-shape zero-tensor zeroes

 ;; Model creation
 model


 ;; Classification accuracy
 accuracy

 ;; Helpers
 trace-print check-dual-equal? check-ρ-∇
 )
