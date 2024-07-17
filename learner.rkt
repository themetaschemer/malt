#lang racket/base

(require "learner/tensors.rkt")
(require "learner/autodiff.rkt")
(require "learner/ext-ops.rkt")

(define ext1-∇
  (λ args
    (error "ext1-∇ is not provided by the learner implementation")))

(define ext2-∇
  (λ args
    (error "ext2-∇ is not provided by the learner implementation")))

(provide
 tolerance

 len ref refr

 tref tlen tmap list->tensor tensor build-tensor

 (rename-out (ext1 ext1-ρ) (ext2 ext2-ρ))

 ext1-∇ ext2-∇

 dual dual? ρ κ ∇ ∇¹ (rename-out (∇ gradient-of)) map*

 ext1 ext2 prim1 prim2

 (rename-out (ext1 ext1-ρ) (ext2 ext2-ρ))

 scalar? tensor? rank shape reshape trefs

 trace-print check-dual-equal? check-ρ-∇

 (rename-out (d+ +) (d- -) (d* *) (d/ /)
             (d-exp exp) (d-log log) (d-expt expt) (d-sqrt sqrt) (d-sqr sqr)
             (d-sum sum) (d-abs abs) (d*-2-1 *-2-1) (d-argmax argmax)
             (d-max max) (d-sum-cols sum-cols) (d-correlate correlate))

 rectify flatten concat concat-n

 +-ρ --ρ *-ρ /-ρ rectify-ρ
 exp-ρ log-ρ expt-ρ sqrt-ρ sqr-ρ zeroes-ρ
 sum-ρ abs-ρ *-2-1-ρ argmax-ρ
 max-ρ sum-cols-ρ correlate-ρ flatten-ρ concat-ρ

 +-0-0 --0-0 *-0-0 /-0-0 expt-0-0
 exp-0 log-0 abs-0 rectify-0 sqrt-0

 sum-1 argmax-1 max-1 flatten-2 concat-1-1


 =-0-0 <-0-0 <=-0-0 >-0-0 >=-0-0
 =-1 <-1 >-1 <=-1 >=-1 !=-1

 max-tensor-print-length make-printable)
