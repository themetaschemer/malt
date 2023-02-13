#lang racket

(require malt)
(require "morse/data/morse-data.ss")

;;--------------------------------------------
;; Training framework.
;;--------------------------------------------

(define train-morse
  (λ (network)
    (with-hypers
      ((alpha 0.0005)
       (revs 20000)
       (batch-size 8)
       (mu 0.9)
       (beta 0.999))
      (trained-morse (block-fn network) (block-ls network)))))

(define trained-morse
  (λ (classifier theta-shapes)
    (model classifier
      (adam-gradient-descent
        (sampling-obj
          ((with-recording l2-loss)
            classifier)
           morse-train-xs morse-train-ys)
        (init-theta theta-shapes)))))

(define train-and-test-morse
  (λ (network)
    (fprintf (current-error-port) "Accuracy: ~a~%"
      (accuracy
        (train-morse network)
        morse-test-xs morse-test-ys))))

;;--------------------------------------------
;; Fully convolutional network
;;--------------------------------------------

(define fcn-block
  (λ (b m d)
    (block (k-recu 2)
      (list
        (list b m d)
        (list b)
        (list b m b)
        (list b)))))

(define signal-avg-block
  (block signal-avg
    (list)))

(define morse-fcn
  (stack-blocks
    (list
      (fcn-block 4 3 1)
      (fcn-block 8 3 4)
      (fcn-block 16 3 8)
      (fcn-block 26 3 16)
      signal-avg-block)))

;;--------------------------------------------
;; Resnet
;;--------------------------------------------

(define skip
  (λ (f j)
    (λ (t)
      (λ (theta)
         (+ ((f t) theta)
            (correlate (ref theta j) t))))))

(define skip-block
  (λ (ba d b)
    (let ((shape-list (block-ls ba)))
      (block (skip (block-fn ba) (len shape-list))
        (append shape-list
          (list
            (list b 1 d)))))))

(define residual-block
  (λ (b m d)
    (skip-block
      (fcn-block b m d)
      d b)))

(define morse-residual
  (stack-blocks
    (list
      (residual-block 4 3 1)
      (residual-block 8 3 4)
      (residual-block 16 3 8)
      (residual-block 26 3 16)
      signal-avg-block)))

;;--------------------------------------------
;; This makes training progress visible.
;;--------------------------------------------

(start-logging)

(provide morse-train-xs morse-train-ys
         morse-test-xs morse-test-ys)

(provide morse-fcn morse-residual
         train-morse train-and-test-morse)
