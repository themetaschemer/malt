#lang racket

(require (only-in "../tensors/c0-ast.rkt"
                  tpmake-prim1-ρ
                  tpmake-prim2-ρ
                  tpmake-prim1-∇
                  tpmake-prim2-∇))
(require "../tensors.rkt")
(require (only-in "../tensors/c1-racket-runtime.rkt" ext2-∇-result))
(require (only-in "../tensors/c0-ast.rkt" tcomp-ds-ref))
(require "A-autodiff.ss")

(struct prim (ρ-fn ρ-acc-fn ∇-fn ∇-acc-fn shape-fn signature expects-prealloc? proc)
  #:property prop:procedure (λ (this . args)
                              (apply (prim-proc this) args)))

;;TODO: Add new ast nodes for the 4 forces being done in the four preallocated->functional-* functions
(define prim1
  (let ((id 0))
    (λ (ρ-fn ρ-acc-fn ∇-fn ∇-acc-fn [shape (λ (l . r) l)] [expects-prealloc? #f])
      (let ((prim-sign (string-append "p1" (~r id #:base 16))))
        (set! id (add1 id))
        (prim ρ-fn ρ-acc-fn ∇-fn ∇-acc-fn shape prim-sign expects-prealloc?
              (λ (da)
                (prim1-dual (if expects-prealloc?
                                (preallocated->functional-1-ρ ρ-fn ρ-acc-fn prim-sign shape)
                                ρ-fn)
                            (if expects-prealloc?
                                (preallocated->functional-1-∇ ∇-fn ∇-acc-fn prim-sign shape)
                                ∇-fn)
                            da)))))))

;; TODO: Convert the use of force* into the construction of an AST so that we
;; don't prematurely trigger computation.
(define prim1-dual
  (λ (ρ-fn ∇-fn da)
    (let ((ra (ρ da)))
      (dual (ρ-fn ra)
        (λ (d z σ)
          (force*1 (∇-fn ra z)
                   (λ (ga)
                     ((κ da) da ga σ))))))))

(define prim2
  (let ((id 0))
    (λ (ρ-fn ρ-acc-fn ∇-fn ∇-acc-fn [shape (λ (l . r) l)] [expects-prealloc? #f])
      (let ((prim-sign (string-append "p2" (~r id #:base 16))))
        (set! id (add1 id))
        (prim ρ-fn ρ-acc-fn ∇-fn ∇-acc-fn shape prim-sign expects-prealloc?
              (λ (da db)
                (prim2-dual (if expects-prealloc?
                                (preallocated->functional-2-ρ ρ-fn ρ-acc-fn prim-sign shape)
                                ρ-fn)
                            (if expects-prealloc?
                                (preallocated->functional-2-∇ ∇-fn ∇-acc-fn prim-sign shape)
                                ∇-fn)
                            da db)))))))

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
;; Managing flat-optimized and
;; non-flat ρ and ∇ functions
;;----------------------------

(define preallocated->functional-1-ρ
  (λ (ρ-fn ρ-fn-acc prim-sign shape-fn)
    (λ (ra)
      (tpmake-prim1-ρ ρ-fn ρ-fn-acc prim-sign shape-fn ra))))

(define preallocated->functional-1-∇
  (λ (∇-fn ∇-fn-acc prim-sign shape-fn)
    (λ (ra z)
      (tpmake-prim1-∇ ∇-fn ∇-fn-acc prim-sign shape-fn ra z))))

(define preallocated->functional-2-ρ
  (λ (ρ-fn ρ-fn-acc prim-sign shape-fn)
    (λ (ra rb)
      (tpmake-prim2-ρ ρ-fn ρ-fn-acc prim-sign shape-fn ra rb))))

(define preallocated->functional-2-∇
  (λ (∇-fn ∇-fn-acc prim-sign shape-fn)
    (λ (ra rb z)
      (let ((out-ref0 (ext2-∇-result (tcomp-ds-ref #f)))
            (out-ref1 (ext2-∇-result (tcomp-ds-ref #f))))
        (values
         (tpmake-prim2-∇ ∇-fn ∇-fn-acc prim-sign shape-fn ra rb z out-ref0 out-ref1 0)
         (tpmake-prim2-∇ ∇-fn ∇-fn-acc prim-sign shape-fn ra rb z out-ref0 out-ref1 1))))))

;;----------------------------
;; Dualized tensor op creators
;;----------------------------
(define ext1
  (λ (f n)
    (unless (prim? f)
      (error 'ext1-prim "Function to be extended must be a primitive. Found: ~a" f))
    (prim1
     (ext1-ρ (prim-ρ-fn f) (prim-ρ-acc-fn f) n (prim-shape-fn f)
             (prim-expects-prealloc? f) (string-append "r" (prim-signature f)))
     (prim-ρ-acc-fn f)
     (ext1-∇ (prim-∇-fn f) (prim-∇-acc-fn f) n (prim-shape-fn f)
             (prim-expects-prealloc? f) (string-append "n" (prim-signature f)))
     (prim-∇-acc-fn f)
     (prim-shape-fn f)
     #f)))

(define ext2
  (λ (f m n)
    (unless (prim? f)
      (error 'ext2-prim "Function to be extended must be a primitive. Found: ~a" f))
    (prim2
     (ext2-ρ (prim-ρ-fn f) (prim-ρ-acc-fn f) m n (prim-shape-fn f)
             (prim-expects-prealloc? f) (string-append "r" (prim-signature f)))
     (prim-ρ-acc-fn f)
     (ext2-∇ (prim-∇-fn f) (prim-∇-acc-fn f) m n (prim-shape-fn f)
             (prim-expects-prealloc? f) (string-append "n" (prim-signature f)))
     (prim-∇-acc-fn f)
     (prim-shape-fn f)
     #f)))

(provide prim1 prim2 ext1 ext2)
