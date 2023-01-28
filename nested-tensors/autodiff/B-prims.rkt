#lang racket

(require "../tensors.rkt")
(require "A-autodiff.rkt")

(define ρ-function
  (λ (f) (f ρ-function)))

(define ∇-function
  (λ (f) (f ∇-function)))

(define prim1
  (λ (ρ-fn ∇-fn)
    (λ (daf)
      (cond
        ((eq? daf ρ-function) ρ-fn)
        ((eq? daf ∇-function) ∇-fn)
        (else (prim1-dual ρ-fn ∇-fn daf))))))

(define prim1-dual
  (λ (ρ-fn ∇-fn da)
    (let ((ra (ρ da)))
      (dual (ρ-fn ra)
        (λ (d z σ)
          (let ((ga (∇-fn ra z)))
            ((κ da) da ga σ)))))))

(define prim2
  (λ (ρ-fn ∇-fn)
    (λ ds
      (let ((daf (ref ds 0)))
        (cond
          ((eq? daf ρ-function) ρ-fn)
          ((eq? daf ∇-function) ∇-fn)
          (else (prim2-dual ρ-fn ∇-fn daf (ref ds 1))))))))

(define prim2-dual
  (λ (ρ-fn ∇-fn da db)
    (let ((ra (ρ da))
          (rb (ρ db)))
      (dual (ρ-fn ra rb)
        (λ (d z σ)
          (let-values (((ga gb) (∇-fn ra rb z)))
            (let ((σ-hat ((κ da) da ga σ)))
              ((κ db) db gb σ-hat))))))))

;;----------------------------
;; Dualized tensor op creators
;;----------------------------
(define ext1
  (λ (f n)
    (prim1
     (ext1-ρ (ρ-function f) n)
     (ext1-∇ (∇-function f) n))))

(define ext2
  (λ (f m n)
    (prim2
     (ext2-ρ (ρ-function f) m n)
     (ext2-∇ (∇-function f) m n))))

(provide prim1 prim2 ext1 ext2)
