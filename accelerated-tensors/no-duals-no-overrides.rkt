#lang racket/base

(module+ test
  (require rackunit))

(require "tensors.rkt")
(require "ext-ops.rkt")

(define scalar? number?)

(provide
 ;; From tensors
 len ref refr

 tref tlen list->tensor tensor build-tensor

 ext1-ρ ext2-ρ

 scalar? tensor? rank shape reshape trefs

 ;; From ext-ops
 +-ρ --ρ *-ρ /-ρ rectify-ρ
 exp-ρ log-ρ expt-ρ sqrt-ρ sqr-ρ
 sum-ρ abs-ρ *-2-1-ρ argmax-ρ
 max-ρ sum-cols-ρ correlate-ρ
 concat-ρ concat-n-ρ flatten-ρ

 =-0-0 <-0-0 <=-0-0 >-0-0 >=-0-0
 =-1 <-1 >-1 <=-1 >=-1 !=-1)
