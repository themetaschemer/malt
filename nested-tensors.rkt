#lang racket/base

(require
 (except-in "nested-tensors/tensors.rkt"
   rank shape reshape trefs tensor? tlen ref refr))

(require "nested-tensors/autodiff.rkt")
(require "nested-tensors/ext-ops.rkt")

(provide
 len ref refr

 tref tlen tmap list->tensor tensor build-tensor

 ext1-ρ ext2-ρ ext1-∇ ext2-∇

 dual dual? ρ κ ∇ ∇¹

 ext1 ext2 prim1 prim2

 scalar? tensor? rank shape reshape trefs

 trace-print check-dual-equal? check-ρ-∇

 (rename-out (d+ +) (d- -) (d* *) (d/ /) (d-rectify rectify)
             (d-exp exp) (d-log log) (d-expt expt) (d-sqrt sqrt) (d-sqr sqr)
             (d-sum sum) (d-abs abs) (d*-2-1 *-2-1) (d-argmax argmax)
             (d-max max) (d-sum-cols sum-cols) (d-correlate correlate))

 +-ρ --ρ *-ρ /-ρ rectify-ρ
 exp-ρ log-ρ expt-ρ sqrt-ρ sqr-ρ
 sum-ρ abs-ρ *-2-1-ρ argmax-ρ
 max-ρ sum-cols-ρ correlate-ρ

 =-0-0 <-0-0 <=-0-0 >-0-0 >=-0-0
 =-1 <-1 >-1 <=-1 >=-1 !=-1)
