#lang racket
(require "../../flat-tensors/ext-impl.rkt")

;; tensor computations
(struct tcomp () #:transparent)
#;
(: lst (U (Listof tpromise) (Listof Number)))
(struct tcomp-list->tensor tcomp (lst) #:transparent)
#;
(: s (Listof Natural)) ;; non-empty
#;
(: f (-> (Listof Natural) Number))
(struct tcomp-build-tensor tcomp (s f) #:transparent)
#;
(: tp tpromise)
#;
(: i Natural)
(struct tcomp-tref tcomp (tp i) #:transparent)
#;
(: tp tpromise)
#;
(: i (Listof Natural))
(struct tcomp-trefs tcomp (tp b) #:transparent)
#;
(: fᵈ (U (-> Number Number (Values Number Number))
         (-> (Vector Number) Natural (Listof Natural)
             (Vector Number) Natural (Listof Natural)
             (Vector Number) Natural (Listof Natural))))
(struct tcomp-ext1-ρ-scalar tcomp (f tp) #:transparent)
(struct tcomp-ext1-ρ tcomp (f m shape-fn tp) #:transparent)
(struct tcomp-ext2-ρ-scalar tcomp (f tp-t tp-u) #:transparent)
(struct tcomp-ext2-ρ tcomp (tp-t tp-u f m n shape-fn) #:transparent)
(struct tcomp-ext1-∇ tcomp (tp zp f m shape-fn) #:transparent)
(struct tcomp-ext2-∇ tcomp (fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
  #:transparent)
(struct tcomp-reshape tcomp (s tp) #:transparent)
(struct tcomp-let tcomp (lhs rhs body) #:transparent)
(struct tcomp-var tcomp (name) #:transparent)
(struct tcomp-ds-ref tcomp (index) #:transparent)

(struct tpromise ((tensor #:mutable) shape)
  #:guard
  (λ (tensor shape name)
    (unless (or (flat? tensor) (tcomp? tensor))
      (error 'make-tpromise
             (string-append
              "First argument must be either a"
              " tcomp or a flat tensor. Got ~a")
             tensor))
    (unless ((listof positive-integer?) shape)
      (error 'make-tpromise
             (string-append
              "Second argument must be a list"
              " of positive integers. Got ~a")
             shape))
    (values tensor shape))
  #:transparent)

(provide (struct-out tcomp)
         (struct-out tcomp-list->tensor)
         (struct-out tcomp-build-tensor)
         (struct-out tcomp-tref)
         (struct-out tcomp-trefs)
         (struct-out tcomp-ext1-ρ-scalar)
         (struct-out tcomp-ext1-ρ)
         (struct-out tcomp-ext2-ρ-scalar)
         (struct-out tcomp-ext2-ρ)
         (struct-out tcomp-ext1-∇)
         (struct-out tcomp-ext2-∇)
         (struct-out tcomp-reshape)
         (struct-out tcomp-let)
         (struct-out tcomp-var)
         (struct-out tcomp-ds-ref)
         (struct-out tpromise))
