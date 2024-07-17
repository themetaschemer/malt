#lang racket
(require "../../accelerated-tensors/ext-impl.rkt")
(require (prefix-in acc: "../../accelerated-tensors/tensors.rkt"))
(require file/xxhash32)

;; tensor computations
(struct tcomp () #:transparent)
#;
(: lst (U (Listof tpromise) (Listof Number)))
(struct tcomp-list->tensor tcomp (lst) #:transparent)
#;
(: s (Listof Natural)) ;; non-empty
#;
(: f (-> (Listof Natural) Number))
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
(struct tcomp-ext1-ρ-scalar tcomp (f f-acc sign tp) #:transparent)
(struct tcomp-ext1-ρ tcomp (f f-acc sign m shape-fn tp) #:transparent)
(struct tcomp-ext2-ρ-scalar tcomp (f f-acc sign tp-t tp-u) #:transparent)
(struct tcomp-ext2-ρ tcomp (tp-t tp-u f f-acc sign m n shape-fn) #:transparent)
(struct tcomp-ext1-∇ tcomp (tp zp f f-acc sign m shape-fn) #:transparent)
(struct tcomp-ext2-∇ tcomp (fᵈ fᵈ-acc
                            sign r0 r1 shape-fn
                            tp-t0 tp-t1 tp-z
                            out-ref0 out-ref1 i)
  #:transparent)
(struct tcomp-prim1-ρ tcomp (f f-acc sign shape-fn tp) #:transparent)
(struct tcomp-prim1-∇ tcomp (f f-acc sign shape-fn tp zp) #:transparent)
(struct tcomp-prim2-ρ tcomp (f f-acc sign shape-fn tp-t tp-u) #:transparent)
(struct tcomp-prim2-∇ tcomp (f f-acc sign shape-fn tp-t tp-u zp out-ref0 out-ref1 i) #:transparent)
(struct tcomp-reshape tcomp (s tp) #:transparent)
(struct tcomp-let tcomp (lhs rhs body) #:transparent)
(struct tcomp-var tcomp (name) #:transparent)
(struct tcomp-ds-ref tcomp (index) #:transparent)

(struct tpromise ((tensor #:mutable) shape dst (sign #:mutable))
  #:guard
  (λ (tensor shape data-segment-tree signature name)
    (unless (or (tcomp? tensor) (number? tensor))
      (error 'make-tpromise
             (string-append
              "First argument must be either a"
              " number or a tcomp. Got ~a")
             tensor))
    (unless ((listof positive-integer?) shape)
      (error 'make-tpromise
             (string-append
              "Second argument must be a list"
              " of positive integers. Got ~a")
             shape))
    (unless (and (box? data-segment-tree)
                 (list? (unbox data-segment-tree)))
      (error 'make-tpromise
             (string-append
              "Third argument must be a box containing a list. Got ~a")
             data-segment-tree))
    (unless (and (box? signature) (list? (unbox signature)))
      (error 'make-tpromise
             (string-append
              "Fourth argument must be a box containing a list."
              " Got ~a")
             signature))
    (values tensor shape data-segment-tree signature))
  #:transparent)

(define dst->data-segment
  (λ (dst)
    (apply vector-append (map dst-member->ds (unbox dst)))))

(define dst-member->ds
  (λ (dstm)
    (cond
      ((or (eqv? dstm 'uncalculated)
           (number? dstm) (flat? dstm))
       (vector dstm))
      ((box? dstm) (dst->data-segment dstm))
      (else (error 'malformed-dst-member "Invalid signature. Got ~a" dstm)))))

(define gdst-list->tensor
  (λ (lst)
    (box
     (for/list ((l lst)
                #:when (tpromise? l))
       (tpromise-dst l)))))

(define gdst-tref
  (λ (tp i)
    (box (list (tpromise-dst tp) i))))

(define gdst-trefs
  (λ (tp i-lst)
    (box (list (tpromise-dst tp) (acc:list->tensor i-lst)))))

(define gdst-ext2-∇
  (λ (tp-t0 tp-t1 tp-z)
    (let ((dsn0 (tpromise-dst tp-t0))
          (dsn1 (tpromise-dst tp-t1))
          (dsnz (tpromise-dst tp-z)))
      (box (list dsn0 dsn1 dsnz 'uncalculated)))))

(define sign
  (λ (ss)
    (let ((xxh32-ctx (make-xxh32)))
      (xxh32-reset! xxh32-ctx 0)
      (sign-traverse-list! (unbox ss) xxh32-ctx)
      (xxh32-digest xxh32-ctx))))

(define sign-traverse-list!
  (λ (ss ctx)
    (for ((s ss))
      (sign-traverse-member! s ctx))))

(define sign-traverse-member!
  (λ (s ctx)
    (cond
      ((bytes? s) (xxh32-update! ctx (bytes-append #"_" s)))
      ((box? s) (sign-traverse-list! (unbox s) ctx))
      (else (error 'malformed-sign-member "Invalid signature. Got ~a" s)))))

(define number->bytes
  (λ (n)
    (string->bytes/utf-8 (number->string n))))

(define string->bytes string->bytes/utf-8)

(define gs-list->tensor
  (λ (lst)
    (box (list* #"l>t" (map (λ (l)
                             (cond
                               ((tpromise? l) (tpromise-sign l))
                               ((number? l) (bytes-append #"s_" (number->bytes l)))
                               (else (error 'gs-list->tensor "Unexpected: ~a" l))))
                           lst)))))

(define gs-tref
  (λ (tp)
    (box (list #"tr" (tpromise-sign tp) #"dsr"))))

(define gs-trefs
  (λ (tp)
    (box (list #"trs" (tpromise-sign tp) #"dsr"))))

(define gs-ext1-ρ-scalar
  (λ (signature tp)
    (box (list #"e1rs" (string->bytes signature) (tpromise-sign tp)))))

(define gs-ext1-ρ
  (λ (signature m tp)
    (box (list #"e1r" (string->bytes signature)
               (number->bytes m) (tpromise-sign tp)))))

(define gs-ext2-ρ-scalar
  (λ (signature tp-t tp-u)
    (box (list #"e2rs" (string->bytes signature)
               (tpromise-sign tp-t) (tpromise-sign tp-u)))))

(define gs-ext2-ρ
  (λ (signature m n tp-t tp-u)
    (box (list #"e2r" (string->bytes signature) (number->bytes m) (number->bytes n)
               (tpromise-sign tp-t) (tpromise-sign tp-u)))))

(define gs-ext1-∇
  (λ (signature m tp zp)
    (box (list #"e1n" (string->bytes signature) (number->bytes m)
               (tpromise-sign tp) (tpromise-sign zp)))))

(define gs-ext2-∇
  (λ (signature r0 r1 tp-t0 tp-t1 tp-z i)
    (box (list #"e2n" (string->bytes signature)
               (number->bytes r0) (number->bytes r1)
               (tpromise-sign tp-t0) (tpromise-sign tp-t1) (tpromise-sign tp-z)
               #"dsr" (number->bytes i)))))

(define gs-prim1-ρ
  (λ (prim-sign tp)
    (box (list #"p1r" (string->bytes prim-sign) (tpromise-sign tp)))))

(define gs-prim2-ρ
  (λ (signature tp-t tp-u)
    (box (list #"p2r" (string->bytes signature)
               (tpromise-sign tp-t) (tpromise-sign tp-u)))))

(define gs-prim1-∇
  (λ (signature tp zp)
    (box (list #"p1n" (string->bytes signature) (tpromise-sign tp) (tpromise-sign zp)))))

(define gs-prim2-∇
  (λ (signature tp-t0 tp-t1 tp-z i)
    (box (list #"p2n" (string->bytes signature)
               (tpromise-sign tp-t0) (tpromise-sign tp-t1) (tpromise-sign tp-z)
               #"dsr" (number->bytes i)))))

(define gs-reshape
  (λ (shape tp)
    (box (list* #"r" (tpromise-sign tp) (map number->bytes shape)))))

(define tpromise-flat?
  (λ (v)
    (and (tpromise? v)
         (tcomp-ds-ref? (tpromise-tensor v))
         (flat? (car (unbox (tpromise-dst v)))))))

(define tpmake-flat
  (λ (ft)
    (tpromise (tcomp-ds-ref #f) (flat-shape ft)
                   (box (list ft)) (box (list #"dsr")))))

(define tpmake-list->tensor
  (λ (lst shape)
    (let ((tcomp-node (tcomp-list->tensor lst)))
      (tpromise tcomp-node shape
                (gdst-list->tensor lst)
                (gs-list->tensor lst)))))

(define tpmake-tref
  (λ (tp i shape)
    (tpromise (tcomp-tref tp (tcomp-ds-ref #f))
              shape
              (gdst-tref tp i)
              (gs-tref tp))))

(define tpmake-trefs
  (λ (tp b shape)
    (tpromise (tcomp-trefs tp (tcomp-ds-ref #f)) shape
              (gdst-trefs tp b)
              (gs-trefs tp))))

(define tpmake-ext1-ρ-scalar
  (λ (f f-acc prim-sign tp shape)
    (tpromise (tcomp-ext1-ρ-scalar f f-acc prim-sign tp) shape
              (box (list (tpromise-dst tp)))
              (gs-ext1-ρ-scalar prim-sign tp))))

(define tpmake-ext1-ρ
  (λ (f f-acc prim-sign m shape-fn tp shape)
    (tpromise (tcomp-ext1-ρ f f-acc prim-sign m shape-fn tp)
              shape
              (box (list (tpromise-dst tp)))
              (gs-ext1-ρ prim-sign m tp))))

(define tpmake-ext2-ρ-scalar
  (λ (f f-acc prim-sign tp-t tp-u shape)
    (tpromise (tcomp-ext2-ρ-scalar f f-acc prim-sign tp-t tp-u)
              shape
              (box (list (tpromise-dst tp-t) (tpromise-dst tp-u)))
              (gs-ext2-ρ-scalar prim-sign tp-t tp-u))))

(define ensure-tpromise
  (λ (v)
    (cond
      ((number? v) (tpmake-flat (ensure-flat v)))
      ((flat? v) (tpmake-flat v))
      (else v))))

(define tpmake-ext2-ρ
  (λ (tp-t tp-u f f-acc prim-sign m n shape-fn shape)
    (let ((tp-t (ensure-tpromise tp-t))
          (tp-u (ensure-tpromise tp-u)))
      (tpromise
       (tcomp-ext2-ρ tp-t tp-u f f-acc prim-sign m n shape-fn)
       shape
       (box (list (tpromise-dst tp-t) (tpromise-dst tp-u)))
       (gs-ext2-ρ prim-sign m n tp-t tp-u)))))

;; we invoke ensure-tpromise on just zp because it's the result of calling
;; force*1 which forces zp to be a non-tpromise value. We can ensure tp to
;; be a tpromise as well, but currently in our workflow we never force tp
;; before passing it to this function, nor do we need scalar tp to be wrapped in
;; a tpromise.
(define tpmake-ext1-∇
  (λ (tp zp f f-acc prim-sign m shape-fn shape)
    (let ((zp (ensure-tpromise zp)))
      (tpromise
       (tcomp-ext1-∇ tp zp f f-acc prim-sign m shape-fn)
       shape
       (box (list (tpromise-dst tp) (tpromise-dst zp)))
       (gs-ext1-∇ prim-sign m tp zp)))))

(define tpmake-ext2-∇
  (λ (fᵈ fᵈ-acc prim-sign r0 r1 shape-fn tp-t0 tp-t1 tp-z out-ref0 out-ref1 i shape)
    (let ((tp-t0 (ensure-tpromise tp-t0))
          (tp-t1 (ensure-tpromise tp-t1))
          (tp-z (ensure-tpromise tp-z)))
      (tpromise
       (tcomp-ext2-∇ fᵈ fᵈ-acc prim-sign r0 r1 shape-fn
                     tp-t0 tp-t1 tp-z out-ref0 out-ref1 i)
       shape
       (gdst-ext2-∇ tp-t0 tp-t1 tp-z)
       (gs-ext2-∇ prim-sign r0 r1 tp-t0 tp-t1 tp-z i)))))

(define tpmake-prim1-ρ
  (λ (f f-acc prim-sign shape-fn tp)
    (tpromise (tcomp-prim1-ρ f f-acc prim-sign shape-fn tp)
              (shape-fn (tpromise-shape tp))
              (box (list (tpromise-dst tp)))
              (gs-prim1-ρ prim-sign tp))))

(define tpmake-prim2-ρ
  (λ (f f-acc prim-sign shape-fn tp-t tp-u)
    (tpromise
       (tcomp-prim2-ρ f f-acc prim-sign shape-fn tp-t tp-u)
       (shape-fn (tpromise-shape tp-t) (tpromise-shape tp-u))
       (box (list (tpromise-dst tp-t) (tpromise-dst tp-u)))
       (gs-prim2-ρ prim-sign tp-t tp-u))))

(define tpmake-prim1-∇
  (λ (f f-acc prim-sign shape-fn tp zp)
    (let ((zp (ensure-tpromise zp)))
      (tpromise
       (tcomp-prim1-∇ f f-acc prim-sign shape-fn tp zp)
       (tpromise-shape tp)
       (box (list (tpromise-dst tp) (tpromise-dst zp)))
       (gs-prim1-∇ prim-sign tp zp)))))

(define tpmake-prim2-∇
  (λ (fᵈ fᵈ-acc prim-sign shape-fn tp-t0 tp-t1 tp-z out-ref0 out-ref1 i)
    (let ((tp-t0 (ensure-tpromise tp-t0))
          (tp-t1 (ensure-tpromise tp-t1))
          (tp-z (ensure-tpromise tp-z)))
      (tpromise
       (tcomp-prim2-∇ fᵈ fᵈ-acc prim-sign shape-fn
                     tp-t0 tp-t1 tp-z out-ref0 out-ref1 i)
       (if (zero? i) (tpromise-shape tp-t0) (tpromise-shape tp-t1))
       (gdst-ext2-∇ tp-t0 tp-t1 tp-z) ;; dst constucted in the same way as ext2-∇
       (gs-prim2-∇ prim-sign tp-t0 tp-t1 tp-z i)))))

(define tpmake-reshape
  (λ (tp shape)
    (tpromise
     (tcomp-reshape shape tp) shape
     (tpromise-dst tp)
     (gs-reshape shape tp))))

(provide (struct-out tcomp)
         (struct-out tcomp-list->tensor)
         (struct-out tcomp-tref)
         (struct-out tcomp-trefs)
         (struct-out tcomp-ext1-ρ-scalar)
         (struct-out tcomp-ext1-ρ)
         (struct-out tcomp-ext2-ρ-scalar)
         (struct-out tcomp-ext2-ρ)
         (struct-out tcomp-ext1-∇)
         (struct-out tcomp-ext2-∇)
         (struct-out tcomp-prim1-ρ)
         (struct-out tcomp-prim2-ρ)
         (struct-out tcomp-prim1-∇)
         (struct-out tcomp-prim2-∇)
         (struct-out tcomp-reshape)
         (struct-out tcomp-let)
         (struct-out tcomp-var)
         (struct-out tcomp-ds-ref)
         (struct-out tpromise)
         dst->data-segment
         sign
         number->bytes
         tpromise-flat?
         tpmake-flat
         tpmake-list->tensor
         tpmake-tref
         tpmake-trefs
         tpmake-ext1-ρ-scalar
         tpmake-ext1-ρ
         tpmake-ext2-ρ-scalar
         tpmake-ext2-ρ
         tpmake-ext1-∇
         tpmake-ext2-∇
         tpmake-prim1-ρ
         tpmake-prim2-ρ
         tpmake-prim1-∇
         tpmake-prim2-∇
         tpmake-reshape)
