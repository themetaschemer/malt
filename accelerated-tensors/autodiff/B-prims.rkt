#lang racket

(require "../tensors/0-vectors.rkt")
(require "../tensors.rkt")
(require "A-autodiff.ss")

(define ρ-function
  (λ (f) (f ρ-function)))

(define ρ-acc-function
  (λ (f) (f ρ-acc-function)))

(define ∇-function
  (λ (f) (f ∇-function)))

(define ∇-acc-function
  (λ (f) (f ∇-acc-function)))

(define shape-fn
  (λ (f) (f shape-fn)))

(define signature
  (λ (f) (f signature)))

;; For flat tensors, ρ-fn and ∇-fn
;; are of two types: functional and pre-allocated
;; When they are functional, they return values
;; When they are pre-allocated, they expect expect the
;; return flat-store to be pre-allocated, and simply
;; operate as fillers.
;;
;; Pre-allocated ρ and ∇ have arities
;; 6 and 7 for unary ops, and 9 and 10 for binary ops.
;; We test for this arity to determine the type.
;;
;; Generally speaking, scalar operations are functional
;; and vector operations are pre-allocated.
;;
;; The functions ensure-ρ-callable-1, ensure-∇-callable-1
;; and ensure-ρ-callable-2, ensure-∇-callable-2 provide
;; the preallocation for flat-stores when a vector-op is
;; provided, but the invocation of prim1 expects functional
;; results.
;;

;; Primitives need a unique identifier (its signature) which corresponds to a
;; unique GPU kernel name. However, for the graphics driver in a 2017 macbook
;; pro, the kernel name is limited to a length of 15 characters. This limitation
;; therefore limits how many unique primitives can be created.

(define prim1
  (let ((id 0))
    (λ (ρ-fn ρ-acc-fn ∇-fn ∇-acc-fn [shape (λ (l . r) l)])
      (let ((ρ-callable (ensure-ρ-callable-1 ρ-fn shape))
            (∇-callable (ensure-∇-callable-1 ∇-fn shape))
            (prim-sign (string-append "p1" (~r id #:base 16))))
        (set! id (add1 id))
        (λ (daf)
          (cond
            ((eq? daf ρ-function) ρ-fn)
            ((eq? daf ρ-acc-function) ρ-acc-fn)
            ((eq? daf ∇-function) ∇-fn)
            ((eq? daf ∇-acc-function) ∇-acc-fn)
            ((eq? daf shape-fn) shape)
            ((eq? daf signature) prim-sign)
            (else (prim1-dual ρ-callable ∇-callable daf))))))))

(define prim1-dual
  (λ (ρ-fn ∇-fn da)
    (let ((ra (ρ da)))
      (dual (ρ-fn ra)
        (λ (d z σ)
          (let ((ga (∇-fn ra z)))
            ((κ da) da ga σ)))))))

(define prim2
  (let ((id 0))
    (λ (ρ-fn ρ-acc-fn ∇-fn ∇-acc-fn [shape (λ (l . r) l)])
      (let ((ρ-callable (ensure-ρ-callable-2 ρ-fn shape))
            (∇-callable (ensure-∇-callable-2 ∇-fn shape))
            (prim-sign (string-append "p2" (~r id #:base 16))))
        (set! id (add1 id))
        (λ ds
          (let ((daf (ref ds 0)))
            (cond
              ((eq? daf ρ-function) ρ-fn)
              ((eq? daf ρ-acc-function) ρ-acc-fn)
              ((eq? daf ∇-function) ∇-fn)
              ((eq? daf ∇-acc-function) ∇-acc-fn)
              ((eq? daf shape-fn) shape)
              ((eq? daf signature) prim-sign)
              (else (prim2-dual ρ-callable ∇-callable daf (ref ds 1))))))))))

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
;; Managing flat-optimized and
;; non-flat ρ and ∇ functions
;;----------------------------

(define ensure-ρ-callable-1
  (λ (ρ-fn shape-fn)
    (cond
      ((expects-preallocated? ρ-fn)
       (λ (ra)
         (apply-flat-ρ-fn-1 ρ-fn ra shape-fn)))
      (else ρ-fn))))

(define ensure-∇-callable-1
  (λ (∇-fn shape-fn)
    (cond
      ((expects-preallocated? ∇-fn)
       (λ (ra z)
         (apply-flat-∇-fn-1 ∇-fn ra z shape-fn)))
      (else ∇-fn))))

(define ensure-ρ-callable-2
  (λ (ρ-fn shape-fn)
    (cond
      ((expects-preallocated? ρ-fn)
       (λ (ra rb)
         (apply-flat-ρ-fn-2 ρ-fn ra rb shape-fn)))
      (else ρ-fn))))

(define ensure-∇-callable-2
  (λ (∇-fn shape-fn)
    (cond
      ((expects-preallocated? ∇-fn)
       (λ (ra rb z)
         (apply-flat-∇-fn-2 ∇-fn ra rb z shape-fn)))
      (else ∇-fn))))

(define apply-flat-ρ-fn-1
  (λ (ρ-fn ra shape-fn)
    (let* ((in-shape (flat-shape ra))
           (in-size (size-of in-shape))
           (out-shape (shape-fn in-shape))
           (out-size (size-of out-shape)))
      (cond
        ((null? out-shape)
         (let ((v-out (new-vec 1 0.0)))
           (ρ-fn (flat-store ra) (flat-offset ra) in-size
                 v-out 0 1)
           (vref v-out 0)))
        (else
         (let ((v-out (new-vec out-size 0.0)))
           (ρ-fn (flat-store ra) (flat-offset ra) in-size
                 v-out 0 out-size)
           (flat out-shape v-out 0)))))))

(define apply-flat-∇-fn-1
  (λ (∇-fn ra z shape-fn)
    (let* ((in-shape (flat-shape ra))
           (in-size (size-of in-shape))
           (out-shape (shape-fn in-shape))
           (out-size (size-of out-shape)))
      (let ((g (new-vec in-size 0.0)))
        (cond
          ((null? out-shape)
           (let ((v-z (new-vec 1 z)))
             (∇-fn g (flat-store ra) (flat-offset ra) in-size
                   v-z 0 1)
             (flat in-shape g 0)))
          (else
           (∇-fn g (flat-store ra) (flat-offset ra) in-size
                 (flat-store z) (flat-offset z) out-size)
           (flat in-shape g 0)))))))

(define apply-flat-ρ-fn-2
  (λ (ρ-fn ra rb shape-fn)
    (let* ((in-shape-a (flat-shape ra))
           (in-size-a (size-of in-shape-a))
           (in-shape-b (flat-shape rb))
           (in-size-b (size-of in-shape-b))
           (out-shape (shape-fn in-shape-a in-shape-b))
           (out-size (size-of out-shape)))
      (cond
        ((null? out-shape)
         (let ((v-out (new-vec 1 0.0)))
           (ρ-fn
            (flat-store ra) (flat-offset ra) in-size-a
            (flat-store rb) (flat-offset rb) in-size-b
            v-out 0 1)
           (vref v-out 0)))
        (else
         (let ((v-out (new-vec out-size 0.0)))
           (ρ-fn
             (flat-store ra) (flat-offset ra) in-size-a
             (flat-store rb) (flat-offset rb) in-size-b
             v-out 0 out-size)
           (flat out-shape v-out 0)))))))

(define apply-flat-∇-fn-2
  (λ (∇-fn ra rb z shape-fn)
    (let* ((in-shape-a (flat-shape ra))
           (in-size-a (size-of in-shape-a))
           (in-shape-b (flat-shape rb))
           (in-size-b (size-of in-shape-b))
           (out-shape (shape-fn in-shape-a in-shape-b))
           (out-size (size-of out-shape)))
      (let ((g0 (new-vec in-size-a 0.0))
            (g1 (new-vec in-size-b 0.0)))
        (cond
          ((null? out-shape)
           (let ((v-z (new-vec 1 z)))
             (∇-fn g0 g1
               (flat-store ra) (flat-offset ra) in-size-a
               (flat-store rb) (flat-offset rb) in-size-b
               v-z 0 1)
             (values
               (flat in-shape-a g0 0)
               (flat in-shape-b g1 0))))
          (else
           (∇-fn g0 g1
             (flat-store ra) (flat-offset ra) in-size-a
             (flat-store rb) (flat-offset rb) in-size-b
             (flat-store z) (flat-offset z) out-size)
           (values
             (flat in-shape-a g0 0)
             (flat in-shape-b g1 0))))))))

;;----------------------------
;; Dualized tensor op creators
;;----------------------------

;; TODO: Figure out the behaviour when we compose ext* with ext*. Currently we
;; assume that "f" is always a non-extended primitive.
(define ext1
  (λ (f n)
    (prim1
     (ext1-ρ (ρ-function f) (ρ-acc-function f) n (shape-fn f)
             (string-append "r" (signature f)))
     (ρ-acc-function f)
     (ext1-∇ (∇-function f) (∇-acc-function f) n (shape-fn f)
             (string-append "n" (signature f)))
     (∇-acc-function f)
     (shape-fn f))))

(define ext2
  (λ (f m n)
    (prim2
     (ext2-ρ (ρ-function f) (ρ-acc-function f) m n (shape-fn f)
             (string-append "r" (signature f)))
     (ρ-acc-function f)
     (ext2-∇ (∇-function f) (∇-acc-function f) m n (shape-fn f)
             (string-append "n" (signature f)))
     (∇-acc-function f)
     (shape-fn f))))

(provide prim1 prim2 ext1 ext2
         apply-flat-ρ-fn-1
         apply-flat-ρ-fn-2
         apply-flat-∇-fn-1
         apply-flat-∇-fn-2)
