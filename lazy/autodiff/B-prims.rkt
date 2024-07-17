#lang racket

(require (only-in "../../accelerated-tensors/ext-impl.rkt"
                  new-vec
                  apply-flat-ρ-fn-1
                  apply-flat-ρ-fn-2
                  apply-flat-∇-fn-1
                  apply-flat-∇-fn-2))
(require "../tensors.rkt")
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
                (prim1-dual (if #;#f expects-prealloc? (preallocated->functional-1-ρ ρ-fn shape) ρ-fn)
                            (if #;#f expects-prealloc? (preallocated->functional-1-∇ ∇-fn shape) ∇-fn)
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
                (prim2-dual (if expects-prealloc? (preallocated->functional-2-ρ ρ-fn shape) ρ-fn)
                            (if expects-prealloc? (preallocated->functional-2-∇ ∇-fn shape) ∇-fn)
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
  (λ (ρ-fn shape-fn)
    (λ (ra)
      (force*1 ra
               (λ (ra)
                 (apply-flat-ρ-fn-1 ρ-fn ra shape-fn))))))

(define preallocated->functional-1-∇
  (λ (∇-fn shape-fn)
    (λ (ra z)
      (force*2
       (λ ()
         (values ra z))
       (λ (ra z)
         (apply-flat-∇-fn-1 ∇-fn ra z shape-fn))))))

(define preallocated->functional-2-ρ
  (λ (ρ-fn shape-fn)
    (λ (ra rb)
      (force*2
       (λ ()
         (values ra rb))
       (λ (ra rb)
         (apply-flat-ρ-fn-2 ρ-fn ra rb shape-fn))))))

(define preallocated->functional-2-∇
  (λ (∇-fn shape-fn)
    (λ (ra rb z)
      (force*2
       (λ ()
         (values ra rb))
       (λ (ra rb)
         (force*1
          z
          (λ (z)
            (apply-flat-∇-fn-2 ∇-fn ra rb z shape-fn))))))))

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
