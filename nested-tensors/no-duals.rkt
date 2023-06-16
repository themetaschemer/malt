#lang racket/base

(module+ test
  (require rackunit))

(require "tensors.rkt")
(require "ext-ops.rkt")

(define scalar? number?)

(provide
 ;; From tensors
 len ref refr

 tref tlen tmap list->tensor tensor build-tensor

 ext1-ρ ext2-ρ

 scalar? tensor? rank shape reshape trefs

  ;; From ext-ops
 (rename-out (+-ρ +) (--ρ -) (*-ρ *) (/-ρ /) (rectify-ρ rectify)
             (exp-ρ exp) (log-ρ log) (expt-ρ expt) (sqrt-ρ sqrt) (sqr-ρ sqr)
             (sum-ρ sum) (abs-ρ abs) (*-2-1-ρ *-2-1) (argmax-ρ argmax)
             (max-ρ max) (sum-cols-ρ sum-cols) (correlate-ρ correlate)
             (flatten-ρ flatten))

 =-0-0 <-0-0 <=-0-0 >-0-0 >=-0-0
 =-1 <-1 >-1 <=-1 >=-1 !=-1)
