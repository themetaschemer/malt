#lang racket

(require "../tensors.rkt")
(require "../autodiff.rkt")

(require "A-scalar-ops.ss")

(define d*-2-1
  (ext2 d* 2 1))

(include "test/test-C-star-2-1.rkt")

(provide d*-2-1)
