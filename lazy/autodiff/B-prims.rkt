#lang racket

(require "../tensors.rkt")
(require "A-autodiff.ss")

(define ρ-function
  (λ (f) (f ρ-function)))

(define ∇-function
  (λ (f) (f ∇-function)))

(define shape-fn
  (λ (f) (f shape-fn)))

;;TODO: make prim1 and prim2 func-callable structures using the prop:procedure
;;struct property

(struct prim (ρ-fn ∇-fn shape-fn
                     signature ;;autogenerate this before runtime to avoid
                               ;;changing this during runtime
                     proc ;; This will be the prim*-dual func
                     prealloc? ;; use this to redefine expects-preallocated?
                     )
  #:property prop:procedure (λ (this . args)
                              (apply (prim-proc this) args)))

;;TODO: move expects-preallocated?, functional->preallocated-1-ρ,
;;functional->preallocated-1-∇, functional->preallocated-2-ρ,
;;functional->preallocated-2-∇ here because they depend on the representation of
;;prims

(define prim1
  (λ (ρ-fn ∇-fn [shape (λ (l . r) l)])
    (λ (daf)
      (cond
        ((eq? daf ρ-function) ρ-fn)
        ((eq? daf ∇-function) ∇-fn)
        ((eq? daf shape-fn) shape)
        (else (prim1-dual ρ-fn ∇-fn daf))))))

(define prim1-dual
  (λ (ρ-fn ∇-fn da)
    (let ((ra (ρ da)))
      (dual (ρ-fn ra)
        (λ (d z σ)
          (force*1 (∇-fn ra z)
                   (λ (ga)
                     ((κ da) da ga σ))))))))

(define prim2
  (λ (ρ-fn ∇-fn [shape (λ (l . r) l)])
    (λ ds
      (let ((daf (ref ds 0)))
        (cond
          ((eq? daf ρ-function) ρ-fn)
          ((eq? daf ∇-function) ∇-fn)
          ((eq? daf shape-fn) shape)
          (else (prim2-dual ρ-fn ∇-fn daf (ref ds 1))))))))

(define prim2-dual
  (λ (ρ-fn ∇-fn da db)
    (let ((ra (ρ da))
          (rb (ρ db)))
      (dual (ρ-fn ra rb)
        (λ (d z σ)
          (force*2 (λ () (∇-fn ra rb z))
                   (λ (ga gb)
                     (let ((σ-hat ((κ da) da ga σ)))
                       ((κ db) db gb σ-hat)))))))))

;;----------------------------
;; Dualized tensor op creators
;;----------------------------
(define ext1
  (λ (f n)
    (prim1
     (ext1-ρ (ρ-function f) n (shape-fn f))
     (ext1-∇ (∇-function f) n (shape-fn f))
     (shape-fn f))))

(define ext2
  (λ (f m n)
    (prim2
     (ext2-ρ (ρ-function f) m n (shape-fn f))
     (ext2-∇ (∇-function f) m n (shape-fn f))
     (shape-fn f))))

(provide prim1 prim2 ext1 ext2)
