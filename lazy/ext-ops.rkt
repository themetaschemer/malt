#lang racket

(require "ext-ops/A-scalar-ops.rkt")
(require "ext-ops/B-comparators.rkt")
(require "ext-ops/C-star-2-1.rkt")
(require "ext-ops/D-sum.rkt")
(require "ext-ops/E-argmax.rkt")
(require "ext-ops/F-max.rkt")
(require "ext-ops/G-correlate.rkt")
(require "ext-ops/I-flatten.rkt")

(provide d+ d- d* d/
         d-expt d-exp d-log d-abs
         d-rectify d-sqrt d-sqr

         +-0-0 --0-0 *-0-0 /-0-0 expt-0-0
         exp-0 log-0 sqrt-0 abs-0 rectify-0

         +-ρ --ρ *-ρ /-ρ
         expt-ρ exp-ρ log-ρ abs-ρ
         rectify-ρ sqrt-ρ sqr-ρ)

(provide =-0-0 <-0-0 <=-0-0 >-0-0 >=-0-0
         =-1 <-1 >-1 <=-1 >=-1 !=-1)

(provide d*-2-1 *-2-1-ρ)

(provide sum-1 d-sum sum-ρ d-sum-cols sum-cols-ρ)

(provide argmax-1 d-argmax argmax-ρ)

(provide max-1 d-max max-ρ)

(provide correlate-ρ d-correlate)

(provide flatten-2 d-flatten flatten-ρ)
