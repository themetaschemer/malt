#lang racket

(require "B-tensor-basics.ss")
(require "C-tensor-ops.ss")

;;—————————————————–—————————————————–—————————————————–
;; Unary Pointwise extension
;;—————————————————–—————————————————–—————————————————–

(define ext1
  (λ (f n)
    (λ (t)
      (cond
        ((of-rank? n t) (f t))
        (else (tmap (ext1 f n) t))))))

;;—————————————————–—————————————————–—————————————————–
;; Binary Pointwise extension
;;—————————————————–—————————————————–—————————————————–

(define ext2
  (λ (f n m)
    (λ (t u)
      (cond
        ((of-ranks? n t m u) (f t u))
        (else
         (desc (ext2 f n m) n t m u))))))

(define desc
  (λ (g n t m u)
    (cond
      ((of-rank? n t) (desc-u g t u))
      ((of-rank? m u) (desc-t g t u))
      ((= (rank t) (rank u))
       (if (= (tlen t) (tlen u))
           (tmap g t u)
           (error 'ext
              "Shapes are incompatible for ext2: ~a and ~a for min ranks ~a and ~a~%"
              (shape t) (shape u) n m)))
      ((rank> t u) (desc-t g t u))
      ((rank> u t) (desc-u g t u)) ;; Slight variation from the book, to add error checking
    )))

(define desc-t
  (λ (g t u)
    (tmap (λ (e) (g e u)) t)))

(define desc-u
  (λ (g t u)
    (tmap (λ (e) (g t e)) u)))

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
(provide ext1 ext2)
