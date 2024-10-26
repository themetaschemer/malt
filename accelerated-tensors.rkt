#lang racket/base

(require
 (except-in "accelerated-tensors/tensors.rkt"
   rank shape reshape tref trefs tensor? tlen ref refr))

(require "accelerated-tensors/autodiff.rkt")
(require "accelerated-tensors/ext-ops.rkt")

(provide
 tolerance

 len ref refr

 tref tlen list->tensor tensor build-tensor

 ext1-ρ ext2-ρ ext1-∇ ext2-∇

 dual dual? ρ κ ∇ ∇¹ (rename-out (∇ gradient-of)) map*

 ext1 ext2 prim1 prim2

 scalar? tensor? rank shape reshape trefs

 trace-print check-dual-equal? check-ρ-∇
 max-tensor-print-length make-printable

 (rename-out (d+ +) (d- -) (d* *) (d/ /) (d-rectify rectify)
             (d-exp exp) (d-log log) (d-expt expt) (d-sqrt sqrt) (d-sqr sqr)
             (d-sum sum) (d-abs abs) (d*-2-1 *-2-1) (d-argmax argmax)
             (d-max max) (d-sum-cols sum-cols) (d-correlate correlate)
             (d-flatten flatten)
             (d-concat concat) (d-concat-n concat-n))

 +-ρ --ρ *-ρ /-ρ rectify-ρ
 exp-ρ log-ρ expt-ρ sqrt-ρ sqr-ρ zeroes-ρ
 sum-ρ abs-ρ *-2-1-ρ argmax-ρ
 max-ρ sum-cols-ρ correlate-ρ
 flatten-ρ concat-ρ

 +-0-0 --0-0 *-0-0 /-0-0 expt-0-0
 exp-0 log-0 abs-0 rectify-0 sqrt-0

 sum-1 argmax-1 max-1 flatten-2 concat-1-1

 =-0-0 <-0-0 <=-0-0 >-0-0 >=-0-0
 =-1 <-1 >-1 <=-1 >=-1 !=-1)
