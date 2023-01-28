#lang racket

(require "B-tensor-basics.ss")
(require "C-tensor-ops.ss")

;;—————————————————–—————————————————–—————————————————–
;; Unary Pointwise extension, Forward
;;—————————————————–—————————————————–—————————————————–

(define ext1-ρ
  (λ (f n)
    (λ (t)
      (cond
        ((of-rank? n t) (f t))
        (else (tmap (ext1-ρ f n) t))))))

;;—————————————————–—————————————————–—————————————————–
;; Binary Pointwise extension, Forward
;;—————————————————–—————————————————–—————————————————–

(define ext2-ρ
  (λ (f n m)
    (λ (t u)
      (cond
        ((of-ranks? n t m u) (f t u))
        (else
         (desc (ext2-ρ f n m) n t m u))))))

(define desc
  (λ (g n t m u)
    (cond
      ((of-rank? n t) (desc-u g t u))
      ((of-rank? m u) (desc-t g t u))
      ((= (vector-length t) (vector-length u)) (tmap g t u))
      ((rank> t u) (desc-t g t u))
      ((rank> u t) (desc-u g t u))
      (else (error 'ext
              "Shapes are incompatible for ext2: ~a and ~a for min ranks ~a and ~a~%"
              (shape t) (shape u) n m)))))

(define desc-t
  (λ (g t u)
    (tmap (λ (e) (g e u)) t)))

(define desc-u
  (λ (g t u)
    (tmap (λ (e) (g t e)) u)))

;;—————————————————–—————————————————–—————————————————–
;; Unary Pointwise extension, Reverse
;;—————————————————–—————————————————–—————————————————–

(define ext1-∇
  (λ (f-∇ n)
    (λ (t z)
      (cond
        ((of-rank? n t) (f-∇ t z))
        (else (tmap (ext1-∇ f-∇ n) t z))))))

;;—————————————————–—————————————————–—————————————————–
;; Binary Pointwise extension, Reverse
;;—————————————————–—————————————————–—————————————————–

(define ext2-∇
  (λ (f-∇ n m)
    (λ (t u z)
      (cond
        ((of-ranks? n t m u) (f-∇ t u z))
        (else
         (desc-∇ (ext2-∇ f-∇ n m) n t m u z))))))

(define desc-∇
  (λ (g n t m u z)
    (cond
      ((of-rank? n t) (desc-u-∇ g t u z))
      ((of-rank? m u) (desc-t-∇ g t u z))
      ((= (vector-length t) (vector-length u)) (tmap2 g t u z))
      ((rank> t u) (desc-t-∇ g t u z))
      ((rank> u t) (desc-u-∇ g t u z))
      (else (error 'ext
              "Shapes are incompatible for ext2: ~a and ~a for min ranks ~a and ~a~%"
              (shape t) (shape u) n m)))))

(define tmap2
  (λ (g t u z)
    (build-gt-gu (tlen t)
      (λ (i)
        (g (tref t i) (tref u i) (tref z i))))))

(define desc-t-∇
  (λ (g t u z)
    (build-gt-acc-gu (tlen t)
      (λ (i)
        (g (tref t i) u (tref z i))))))

(define desc-u-∇
  (λ (g t u z)
    (build-gu-acc-gt (tlen u)
      (λ (i)
        (g t (tref u i) (tref z i))))))

(define build-gt-gu
  (λ (n f)
    (let ((gt (make-vector n))
          (gu (make-vector n)))
      (fill-gt-gu gt gu f (sub1 n)))))

(define fill-gt-gu
  (λ (gt gu g i)
    (let-values (((gti gui) (g i)))
      (vector-set! gt i gti)
      (vector-set! gu i gui)
      (cond
        ((zero? i) (values gt gu))
        (else (fill-gt-gu gt gu g (sub1 i)))))))

(define build-gt-acc-gu
  (λ (n g)
    (let ((gt (make-vector n))
          (gu 0.0))
      (fill-gt-acc-gu gt g (sub1 n) gu))))

(define fill-gt-acc-gu
  (λ (gt g i gu)
    (let-values (((gti gui) (g i)))
      (vector-set! gt i gti)
      (let ((gu-hat (+! gu gui)))
        (cond
          ((zero? i) (values gt gu-hat))
          (else (fill-gt-acc-gu gt g (sub1 i) gu-hat)))))))

(define build-gu-acc-gt
  (λ (n g)
    (let ((gu (make-vector n))
          (gt 0.0))
      (fill-gu-acc-gt gu g (sub1 n) gt))))

(define fill-gu-acc-gt
  (λ (gu g i gt)
    (let-values (((gti gui) (g i)))
      (vector-set! gu i gui)
      (let ((gt-hat (+! gt gti)))
        (cond
          ((zero? i) (values gt-hat gu))
          (else (fill-gu-acc-gt gu g (sub1 i) gt-hat)))))))

(define +! (ext2-ρ + 0 0))
(define *! (ext2-ρ * 0 0))
(define one-like (ext1-ρ (λ (x) 1.0) 0))

(define of-rank?
  (λ (n t)
    (cond
      ((zero? n) (scalar? t))
      ((scalar? t) #f)
      (else (of-rank? (sub1 n) (tref t 0))))))

(define of-ranks?
  (λ (n t m u)
    (cond
      ((of-rank? n t) (of-rank? m u))
      (else #f))))

(define rank>
  (λ (t u)
    (> (rank t) (rank u))))

(define tmap vector-map)

(include "test/test-D-extend.rkt")
(provide ext1-ρ ext2-ρ ext1-∇ ext2-∇ +! *! one-like)
