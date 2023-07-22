#lang racket/base

(require
 (except-in "tensors.rkt"
   rank shape reshape trefs tref tensor? tlen ref refr))

(require "autodiff.rkt")
(require "ext-ops.rkt")

(provide
 len ref refr

 tref tlen list->tensor tensor build-tensor

 ext1-ρ ext2-ρ ext1-∇ ext2-∇

 dual dual? ρ κ ∇ ∇¹

 ext1 ext2 prim1 prim2

 scalar? tensor? rank shape reshape trefs

 trace-print check-dual-equal? check-ρ-∇
 make-printable

 +-0-0 --0-0 *-0-0 /-0-0 expt-0-0
 exp-0 log-0 abs-0 rectify-0 sqrt-0
 flatten-2 concat-1-1

 d+ d- d* d/ d-rectify
 d-exp d-log d-expt d-sqrt d-sqr
 d-sum d-abs d*-2-1 d-argmax
 d-max d-sum-cols d-correlate
 d-flatten d-concat d-concat-n

 +-ρ --ρ *-ρ /-ρ rectify-ρ
 exp-ρ log-ρ expt-ρ sqrt-ρ sqr-ρ
 sum-ρ abs-ρ *-2-1-ρ argmax-ρ
 max-ρ sum-cols-ρ correlate-ρ
 flatten-ρ concat-ρ concat-n-ρ

 =-0-0 <-0-0 <=-0-0 >-0-0 >=-0-0
 =-1 <-1 >-1 <=-1 >=-1 !=-1)
