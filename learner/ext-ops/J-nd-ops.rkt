#lang racket

(require (only-in "../tensors.rkt" ext1 ext2 ref tlen tref build-tensor))

;;------------------------------------
;; The learner implementation of tensors
;; does not use dualized tensors, so many
;; functions here are simply replicas
;; of their dualized ones, but they
;; are maintained separatey because of
;; compatibility required with the
;; abstract tensor interface
;;------------------------------------


;;------------------------------------
;; non-differentiable extended functions.
;;------------------------------------

(define *-ρ (ext2 * 0 0))
(define +-ρ (ext2 + 0 0))
(define --ρ (ext2 - 0 0))
(define /-ρ (ext2 /  0 0))
(define expt-ρ (ext2 expt 0 0))

(define exp-ρ (ext1 exp 0))
(define log-ρ (ext1 log 0))
(define abs-ρ (ext1 abs 0))

(define sqrt-ρ
  (λ (a)
    (expt-ρ a 1/2)))

(define sqr-ρ
  (λ (x)
    (*-ρ x x)))

(provide +-ρ --ρ *-ρ /-ρ
         expt-ρ exp-ρ log-ρ abs-ρ
         rectify-ρ sqrt-ρ sqr-ρ)

(define *-2-1-ρ
  (ext2 *-ρ 2 1))

(provide *-2-1-ρ)

;;----------------------------
;; Non differentiable sum, for compatibility with
;; the tensor interface
;;----------------------------

(define sum-1-ρ
  (λ (t)
    (summed-ρ t (sub1 (tlen t)) 0.0)))

(define summed-ρ
  (λ (t i a)
    (let ((next-a (+-ρ a (tref t i))))
      (cond
        ((zero? i) next-a)
        (else (summed-ρ t (sub1 i) next-a))))))

(define sum-ρ
  (ext1 sum-1-ρ 1))

(define sum-cols-ρ
  (ext1 sum-1-ρ 2))

(provide sum-ρ sum-cols-ρ)

(require (only-in "E-argmax.rkt" argmax-1))

(define argmax-ρ
  (ext1 argmax-1 1))

(provide argmax-ρ)

(require (only-in "F-max.rkt" max-1))

(define max-ρ
  (ext1 max-1 1))

(provide max-ρ)

;;------------------------------------
;; Non differentiable correlate-3-2-ρ
;; for compatibility with abstract
;; tensor interface
;;------------------------------------

(define correlate-3-2-ρ
  (λ (bank signal)
    (build-tensor (list (tlen signal) (tlen bank))
      (λ (idx)
        (let ((segment (ref idx 0))
              (filter (tref bank (ref idx 1))))
          (correlate-overlap-ρ filter signal segment))))))

(define correlate-overlap-ρ
  (λ (filter signal segment)
    (let ((q (/ (- (tlen filter) 1) 2)))
      (let ((from (- segment q))
            (to (+ segment q)))
        (sum-dp-ρ filter signal from (sub1 (tlen filter)) 0.0)))))

(define sum-dp-ρ
  (λ (filter signal from i a)
    (let ((si (+ from i)))
      (let ((a-hat
             (cond
               ((and (>= si 0) (< si (tlen signal)))
                (+ a
                   (dot-product-ρ (tref signal si)
                                  (tref filter i))))
               (else a))))
        (cond
          ((zero? i) a-hat)
          (else (sum-dp-ρ filter signal from (sub1 i) a-hat)))))))

(define dot-product-ρ
  (λ (t u)
    (dotted-product-ρ t u (sub1 (tlen t)) 0.0)))

(define dotted-product-ρ
  (λ (t u i a)
    (let ((a-hat (+ a (* (tref t i) (tref u i)))))
      (cond
        ((zero? i) a-hat)
        (else (dotted-product-ρ t u (sub1 i) a-hat))))))

(define correlate-ρ
  (ext2 correlate-3-2-ρ 3 2))

(provide correlate-3-2-ρ correlate-ρ)

(require (only-in "H-rectify.rkt" rectify-0))

(define rectify-ρ
  (ext1 rectify-0 0))

(provide rectify-ρ)

(require (only-in "I-flatten.rkt" flatten-2))

(define flatten-ρ
  (ext1 flatten-2 2))

(provide flatten-ρ)

(require (only-in "K-concat.rkt" concat-1-1))

(define concat-ρ
  (ext1 concat-1-1 1))

(provide concat-ρ)
