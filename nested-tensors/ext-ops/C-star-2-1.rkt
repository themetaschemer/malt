#lang racket

(require (only-in "../tensors.rkt" ext2-ρ))
(require "../autodiff.rkt")

(require "A-scalar-ops.ss")

(define d*-1-1
  (ext2 d* 1 1))

(define d*-2-1
  (ext2 d*-1-1 2 1))

(define *-1-1-ρ
  (ext2-ρ *-ρ 1 1))

(define *-2-1-ρ
  (ext2-ρ *-1-1-ρ 2 1))

(include "test/test-C-star-2-1.rkt")

(provide d*-2-1 *-2-1-ρ)
