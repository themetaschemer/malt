#lang racket/base

(require "tensors.rkt")
(require "autodiff.rkt")
(require "ext-ops.rkt")

(provide
 len ref refr

 tref tlen tmap list->tensor tensor build-tensor

 (rename-out (ext1 ext1-ρ) (ext2 ext2-ρ))

 dual dual? ρ κ ∇ ∇¹

 ext1 ext2 prim1 prim2

 scalar? tensor? rank shape reshape trefs

 trace-print check-dual-equal? check-ρ-∇

 d+ d- d* d/ (rename-out (rectify d-rectify))
 d-exp d-log d-expt d-sqrt d-sqr
 d-sum d-abs d*-2-1 d-argmax
 d-max d-sum-cols d-correlate

 +-ρ --ρ *-ρ /-ρ rectify-ρ
 exp-ρ log-ρ expt-ρ sqrt-ρ
 sum-ρ abs-ρ *-2-1-ρ argmax-ρ
 max-ρ sum-cols-ρ correlate-ρ

 =-0-0 <-0-0 <=-0-0 >-0-0 >=-0-0
 =-1 <-1 >-1 <=-1 >=-1 !=-1)
