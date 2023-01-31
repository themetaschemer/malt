#lang racket

(require "malted/A-core.rkt")
(require "malted/B-layer-fns.rkt")
(require "malted/C-loss.rkt")
(require "malted/D-gradient-descent.rkt")
(require "malted/E-gd-common.rkt")
(require "malted/F-naked.rkt")
(require "malted/G-velocity.rkt")
(require "malted/H-rms.rkt")
(require "malted/I-adam.rkt")
(require "malted/J-stochastic.rkt")
(require "malted/K-dense.rkt")
(require "malted/L-accuracy.rkt")
(require "malted/M-recu.rkt")
(require "malted/N-blocks.rkt")
(require "malted/O-init.rkt")

(provide dot-product-2-1 dot-product
         line quad linear-1-1 linear plane softmax
         avg-cols signal-avg
         l2-loss cross-entropy-loss kl-loss
         with-recording

         gradient-descent revise
         revs alpha batch-size
         set-revs! set-alpha! set-batch-size!

         zeroes smooth epsilon
         mu beta
         set-mu! set-beta!

         naked-gradient-descent
         velocity-gradient-descent
         rms-gradient-descent
         adam-gradient-descent

         samples sampling-obj

         relu k-relu

         accuracy model

         recu corr k-recu

         block block-fn block-ls stack-blocks

         init-theta init-shape random-tensor zero-tensor)
