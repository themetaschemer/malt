#lang racket

(require "../tensors.rkt")
(require "A-autodiff.ss")

(struct prim (ρ-fn ∇-fn shape-fn signature expects-prealloc? proc)
  #:property prop:procedure (λ (this . args)
                              (apply (prim-proc this) args)))

(define prim1
  (λ (ρ-fn ∇-fn [shape (λ (l . r) l)] [expects-prealloc? #f])
    (let ((prim-sign (symbol->string (gensym 'prim1))))
      (prim ρ-fn ∇-fn shape prim-sign expects-prealloc?
            (λ (da)
              (prim1-dual ρ-fn ∇-fn da))))))

(define prim1-dual
  (λ (ρ-fn ∇-fn da)
    (let ((ra (ρ da)))
      (dual (ρ-fn ra)
        (λ (d z σ)
          (force*1 (∇-fn ra z)
                   (λ (ga)
                     ((κ da) da ga σ))))))))

(define prim2
  (λ (ρ-fn ∇-fn [shape (λ (l . r) l)] [expects-prealloc? #f])
    (let ((prim-sign (symbol->string (gensym 'prim2))))
      (prim ρ-fn ∇-fn shape prim-sign expects-prealloc?
            (λ (da db)
              (prim2-dual ρ-fn ∇-fn da db))))))

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
    (unless (prim? f)
      (error 'ext1-prim "Function to be extended must be a primitive. Found: ~a" f))
    (prim1
     (ext1-ρ (prim-ρ-fn f) n (prim-shape-fn f)
             (prim-expects-prealloc? f) (prim-signature f))
     (ext1-∇ (prim-∇-fn f) n (prim-shape-fn f)
             (prim-expects-prealloc? f) (prim-signature f))
     (prim-shape-fn f)
     #f)))

(define ext2
  (λ (f m n)
    (unless (prim? f)
      (error 'ext2-prim "Function to be extended must be a primitive. Found: ~a" f))
    (prim2
     (ext2-ρ (prim-ρ-fn f) m n (prim-shape-fn f)
             (prim-expects-prealloc? f) (prim-signature f))
     (ext2-∇ (prim-∇-fn f) m n (prim-shape-fn f)
             (prim-expects-prealloc? f) (prim-signature f))
     (prim-shape-fn f)
     #f)))

(provide prim1 prim2 ext1 ext2)
