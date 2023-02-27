#lang racket/base

(require "tensors.rkt")
(require "autodiff.rkt")
(require "ext-ops.rkt")

(provide
 len ref refr

 tref tlen tmap list->tensor tensor build-tensor
 make-printable

 (rename-out (ext1 ext1-ρ) (ext2 ext2-ρ))

 dual dual? ρ κ ∇ ∇¹

 ext1 ext2 prim1 prim2

 scalar? tensor? rank shape reshape trefs

 trace-print check-dual-equal? check-ρ-∇

 +-0-0 --0-0 *-0-0 /-0-0 expt-0-0
 exp-0 log-0 abs-0 sqrt-0 sum-1
 flatten-2

 d+ d- d* d/ (rename-out (rectify d-rectify))
 d-exp d-log d-expt d-sqrt d-sqr
 d-sum d-abs d*-2-1 d-argmax
 d-max d-sum-cols d-correlate
 (rename-out (flatten d-flatten))

 +-ρ --ρ *-ρ /-ρ rectify-ρ
 exp-ρ log-ρ expt-ρ sqrt-ρ
 sum-ρ abs-ρ *-2-1-ρ argmax-ρ
 max-ρ sum-cols-ρ correlate-ρ
 flatten-ρ

 =-0-0 <-0-0 <=-0-0 >-0-0 >=-0-0
 =-1 <-1 >-1 <=-1 >=-1 !=-1)
