#lang racket
(require "../../flat-tensors/ext-impl.rkt")
(require (prefix-in flat: "../../flat-tensors/tensors.rkt"))


;; tensor computations
(struct tcomp ())
#;
(: lst (U (Listof tpromise) (Listof Number)))
(struct tcomp-list->tpromise-list tcomp (lst) #:transparent)
#;
(: s (Listof Natural)) ;; non-empty
#;
(: f (-> (Listof Natural) Number))
(struct tcomp-build-tpromise tcomp (s f) #:transparent)
#;
(: tp tpromise)
#;
(: i Natural)
(struct tcomp-tp-tref tcomp (tp i) #:transparent)
#;
(: tp tpromise)
#;
(: i (Listof Natural))
(struct tcomp-tp-trefs tcomp (tp b) #:transparent)
;;TODO: Use functional->preallocated-* to use non-mutated/functional types for
;;      the ext base functions
#;
(: fᵈ (U (-> Number Number (Values Number Number))
         (-> (Vector Number) Natural (Listof Natural)
             (Vector Number) Natural (Listof Natural)
             (Vector Number) Natural (Listof Natural))))
(struct tcomp-ext2-∇ tcomp (fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
  #:transparent)
(struct tcomp-ext1-∇-prealloc tcomp (tp zp f m shape-fn) #:transparent)
(struct tcomp-ext1-∇ tcomp (tp zp f m shape-fn) #:transparent)
(struct tcomp-ext2-ρ-scalar tcomp (f tp-t tp-u) #:transparent)
(struct tcomp-ext2-ρ-prealloc tcomp (tp-t tp-u f m n shape-fn) #:transparent)
(struct tcomp-ext2-ρ tcomp (tp-t tp-u f m n shape-fn) #:transparent)
(struct tcomp-ext1-ρ-scalar tcomp (f tp) #:transparent)
(struct tcomp-ext1-ρ-prealloc tcomp (f m shape-fn tp) #:transparent)
(struct tcomp-ext1-ρ tcomp (f m shape-fn tp) #:transparent)
(struct tcomp-reshape tcomp (s tp) #:transparent)
#;
(: args (U (Listof tpromise) (Listof Number)))
(struct tcomp-tensor tcomp (args) #:transparent)

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

#;
(: scalar? (-> Any Boolean))
(define scalar? number?)

#;
(: tensor (case-> (-> tpromise * tpromise)
                  (-> Number * tpromise)))
(define tensor
  (λ args
    (unless (ensure-shape args)
      (error 'tensor
             "Mismatched shapes: ~a~%"
             args))

    (let ((inner-flat (tensor-inner-flat args)))
      (cond
        ((flat? inner-flat)
         (tpromise inner-flat (flat-shape inner-flat)))
        (else
         (let* ((inner-shape (tpromise-shape (car args)))
                (outer (length args))
                (new-shape (cons outer inner-shape)))
           (tpromise inner-flat new-shape)))))))
#;
(: tensor-inner-flat (-> (U (Listof tpromise) (Listof Number))
                       (U flat tcomp)))
(define tensor-inner-flat
  (λ (args)
    (cond
     [(number? (car args)) (apply flat:tensor args)]
     [else (tcomp-tensor args)])))

#;
(: ensure-shape (-> (U (Listof tpromise) (Listof Number)) Boolean))
(define ensure-shape
  (λ (args)
    (and (not (null? args))
         (cond
           ((number? (car args))
            (andmap number? (cdr args)))
           ((tpromise? (car args))
            (let ((s (tp-shape (car args))))
              (andmap (λ (t)
                        (and (tpromise? t)
                             (equal? (tp-shape t) s)))
                      (cdr args))))
           (else #f)))))

#;
(: ensure-flat (-> (U flat Number) flat))
(define ensure-flat
  (λ (v)
    (cond
      ((scalar? v) (flat '() (vec v) 0))
      (else v))))

#;
(: tp-force (-> tpromise (U flat Number)))
(define tp-force
  (lambda (tp (print? #f))
    (when print?
      (printf "~n####PP tensor: ")
      (pretty-print tp))
    (let ([res
           (match tp
             [(tpromise t-tcomp _)
              #:when (tcomp? t-tcomp)
              (tcomp-force t-tcomp)]
             [(tpromise t _)
              #:when (or (flat? t) (scalar? t)) t]

           ;; NOTE: This case runs when we use tp-scalarize to turn
           ;; the tensor to a scalar
             [_ #f])])
      (cond
        [res (set-tpromise-tensor! tp res)
             res]
        [else tp]))))

#;
(: tcomp-force (-> tcomp (U flat Number)))
(define tcomp-force
  (λ (tc)
    (match tc
      [(tcomp-list->tpromise-list lst)
       (flat:list->tensor
        (map (λ (l) (tp-force l #f)) lst))]
      [(tcomp-build-tpromise s f)
       (flat:build-tensor s f)]
      [(tcomp-tp-tref tp i)
       (flat:tref (tp-force tp) i)]
      [(tcomp-tp-trefs tp b)
       (flat:trefs (tp-force tp) b)]
      [(tcomp-ext2-∇ fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
       (let* ([b (if (zero? i) out0 out1)]
              [v (unbox b)])
         (cond
           ((eqv? v 'uncalculated)
            (ext2-∇-forcer fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1)
            (unbox b))
           (else v)))]
      [(tcomp-ext1-∇-prealloc tp zp f m shape-fn)
       (tp-scalarize
        (flat-ext1-∇ f m shape-fn
                     (ensure-flat (tp-force tp))
                     (ensure-flat (tp-force zp))))]
      [(tcomp-ext1-∇ tp zp f m shape-fn)
       (let ([t (ensure-flat (tp-force tp #f))]
             [z (ensure-flat (tp-force zp #f))])
         (let* ((in-shape (flat-shape t))
                (base-shape (min-shape m in-shape))
                (out-shape (shape-fn base-shape))
                (flat-f (functional->preallocated-1-∇ f base-shape out-shape)))
         (tp-scalarize (flat-ext1-∇ flat-f m shape-fn t z))))]
      [(tcomp-ext2-ρ-scalar f tp-t tp-u)
       (f (tp-force tp-t) (tp-force tp-t))]
      [(tcomp-ext2-ρ-prealloc tp-t tp-u f m n shape-fn)
       (tp-scalarize
          (flat-ext2-ρ f m n shape-fn
                       (ensure-flat (tp-force tp-t))
                       (ensure-flat (tp-force tp-u))))]
      [(tcomp-ext2-ρ tp-t tp-u f m n shape-fn)
       (let ([t (ensure-flat (tp-force tp-t #f))]
             [u (ensure-flat (tp-force tp-u #f))])
         (let* ((t-shape (min-shape m (flat-shape t)))
                (u-shape (min-shape n (flat-shape u)))
                (out-shape (shape-fn t-shape u-shape))
                (flat-f (functional->preallocated-2-ρ f t-shape u-shape out-shape)))
           (tp-scalarize
            (flat-ext2-ρ flat-f m n shape-fn t u))))]
      [(tcomp-ext1-ρ-scalar f tp)
       (f (tp-force tp))]
      [(tcomp-ext1-ρ-prealloc f m shape-fn tp)
       (tp-scalarize (flat-ext1-ρ f m shape-fn (ensure-flat (tp-force tp))))]
      [(tcomp-ext1-ρ f m shape-fn tp)
       (let ([t (ensure-flat (tp-force tp #f))])
         (let* ((in-shape (flat-shape t))
                (base-shape (min-shape m in-shape))
                (out-shape (shape-fn base-shape))
                (flat-f (functional->preallocated-1-ρ f base-shape out-shape)))
           (tp-scalarize
            (flat-ext1-ρ flat-f m shape-fn t))))]
      [(tcomp-reshape s tp)
       (let ([t (tp-force tp #f)])
         (flat s (flat-store t) (flat-offset t)))]
      [(tcomp-tensor args)

       (merge-flats (map tp-force args))])))

(define bounded-idx*^
  (λ (shape idx*)
    (match `(,shape ,idx*)
      [`(,_ ()) #t]
      [`(() ,_) #f]
      [`((,sa . ,sd) (,ia . ,id*))
       (and (< ia sa)
            (>= ia 0)
            (bounded-idx*^ sd id*))])))

(define bounded-idx*?
  (λ (tp idx*)
    (bounded-idx*^ (tpromise-shape tp) idx*)))

(define tp-tref
  (lambda (tp i)
    (cond
      [(bounded-idx*? tp (list i))
       (tpromise (tcomp-tp-tref tp i)
                 (cdr (tpromise-shape tp)))]
      [else (error 'exn:tp-tref
                   (string-append
                    "Index out of bounds. ~a "
                    "greater than or equals length ~a~%")
                   i
                   (tp-tlen tp))])))

(define tp-tlen
  (λ (tp)
    (car (tpromise-shape tp))))

(define tp-shape
  (lambda (v)
    (cond
      [(tpromise? v) (tpromise-shape v)]
      [else (flat:shape v)])))

(define list->tpromise
  (λ (lst)
    (cond
      [(null? lst)
       (error 'list->ltensor "No elements found")]
      [else
       (tpromise (tcomp-list->tpromise-list lst)
                 `(,(length lst)
                   . ,(tp-shape
                       (car lst))))])))

(define build-tpromise
  (λ (s f)
    (tpromise (tcomp-build-tpromise s f) s)))

(define tp-trefs
  (λ (tp b)
    (cond
      [(ormap (λ (i)
                (>= i
                    (car (tpromise-shape tp))))
              b)
       (error 'tp-trefs
              "An index was out of bounds")]
      [else
       (tpromise (tcomp-tp-trefs tp b)
                 `(,(length b)
                   . ,(cdr (tpromise-shape tp))))])))

(define tp-ext1-ρ
  (λ (f m [shape-fn scalar-shape])
    (λ (tp)
      (cond
        [(scalar? tp) (f tp)]
        [(and (tpromise? tp)
              (null? (tpromise-shape tp)))
         (tpromise
          (tcomp-ext1-ρ-scalar f tp)
          '())]
        [(flat:expects-preallocated? f)
         (tpromise
          (tcomp-ext1-ρ-prealloc f m shape-fn tp)
          (merge-shapes
           (tp-shape tp)
           m
           (shape-fn
            (min-shape m (tp-shape tp)))))]
        [else
         (tpromise
          (tcomp-ext1-ρ f m shape-fn tp)
          (merge-shapes
           (tp-shape tp)
           m
           (shape-fn
            (min-shape m (tp-shape tp)))))]))))

(define tp-ext2-ρ
  (λ (f m n [shape-fn scalar-shape])
    (λ (tp-t tp-u)
      (cond
        ((and (number? tp-t) (number? tp-u))
         (f tp-t tp-u))
        [(and (tpromise? tp-t) (tpromise? tp-u)
              (null? (tpromise-shape tp-t))
              (null? (tpromise-shape tp-u)))
         (tpromise (tcomp-ext2-ρ-scalar f tp-t tp-u) '())]
        [(flat:expects-preallocated? f)
         (let* ([s0 (tp-shape tp-t)]
                [s1 (tp-shape tp-u)]
                [sf0 (min-shape m s0)]
                [sf1 (min-shape n s1)]
                [sf-out (shape-fn sf0 sf1)])
           (tpromise
            (tcomp-ext2-ρ-prealloc tp-t tp-u
                                   f m n shape-fn)
            (ext2-shapes s0 s1 m n sf-out
                         (λ (s-out . _) s-out))))]
        [else
         (let* ([s0 (tp-shape tp-t)]
                [s1 (tp-shape tp-u)]
                [sf0 (min-shape m s0)]
                [sf1 (min-shape n s1)]
                [sf-out (shape-fn sf0 sf1)])
           (tpromise
            (tcomp-ext2-ρ (ensure-tpromise tp-t) (ensure-tpromise tp-u)
                          f m n shape-fn)
            (ext2-shapes s0 s1 m n sf-out
                         (λ (s-out . _) s-out))))]))))

(define tp-scalarize
  (λ (tp)
    (cond
      [(and (tpromise? tp) (null? (tpromise-shape tp)))
       (tp-scalarize (tp-force tp))]
      [(and (flat? tp) (null? (flat-shape tp)))
       (vref (flat-store tp) 0)]
      [else tp])))

(define scalar-shape
  (λ (s0 [s1 '()]) '()))

(define left-shape
  (λ (s0 s1) s0))

(define right-shape
  (λ (s0 s1) s1))

(define flat-function-maker2
  (λ (f m n)
    (cond
      ((and (zero? m) (zero? n))
       (λ (v0 i0 stride0 v1 i1 stride1
              v-out i-out stride-out)
         (vset! v-out i-out
                (f (vref v0 i0) (vref v1 i1)))))
      (else
       f))))

(define flat-function-maker1
  (λ (f m)
    (cond
      ((zero? m)
       (λ (v0 i0 stride0 v-out i-out stride-out)
         (vset! v-out i-out (f (vref v0 i0)))))
      (else f))))

(define tp-ext1-∇
  (λ (f m [shape-fn scalar-shape])
    (λ (tp zp)
      (cond
        ((number? tp) (f tp zp))
        ((flat:expects-preallocated? f)
         (tpromise
          (tcomp-ext1-∇-prealloc tp zp f m shape-fn)
          (tp-shape tp)))
        (else
         (tpromise
          (tcomp-ext1-∇ tp zp f m shape-fn)
          (tp-shape tp)))))))

(define tp-ext2-∇
  (λ (f m n [shape-fn scalar-shape])
    (let ((tp-f
           (λ (f tp-t tp-u tp-z)
             (tp-d-ext2^ f m n shape-fn
                         tp-t tp-u tp-z))))
      (λ (tp-t tp-u tp-z)
        (cond
          ((flat:expects-preallocated? f)
           (tp-f f tp-t tp-u tp-z))
          [else (let* ((t-shape (min-shape m (tp-shape tp-t)))
                       (u-shape (min-shape n (tp-shape tp-u)))
                       (out-shape (shape-fn t-shape u-shape))
                       (flat-f (functional->preallocated-2-∇
                                f t-shape u-shape out-shape)))
                  (tp-f flat-f
                        (ensure-tpromise tp-t)
                        (ensure-tpromise tp-u)
                        (ensure-tpromise tp-z)))])))))

(define ext2-∇-forcer
  (λ (fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1)
    (let* ((s0 (tp-shape tp-t0))
           (sf0 (min-shape r0 s0))
           (stride0 (flat:size-of sf0))

           (s1 (tp-shape tp-t1))
           (sf1 (min-shape r1 s1))
           (stride1 (flat:size-of sf1))

           (sf-z (shape-fn sf0 sf1))
           (stride-z (flat:size-of sf-z))

           (f0 (ensure-flat (tp-force tp-t0)))
           (f1 (ensure-flat (tp-force tp-t1)))
           (fz (ensure-flat (tp-force tp-z)))

           (v0 (flat-store f0))
           (v1 (flat-store f1))
           (vz (flat-store fz))

           (off0 (flat-offset f0))
           (off1 (flat-offset f1))
           (offz (flat-offset fz)))
      (ext2-shapes
       s0 s1 r0 r1 sf-z
       (λ (sz size-z q0 q1 strides)
         (let ((g0 (new-vec (flat:size-of
                             s0)
                            0.0))
               (g1 (new-vec (flat:size-of
                             s1)
                            0.0)))
           (for ([iz (in-range
                      0
                      size-z
                      stride-z)])
             (let-values (((i0 i1)
                           (idxs
                            strides
                            iz
                            off0
                            off1)))
               (fᵈ g0 g1 v0 i0
                   stride0
                   v1 i1
                   stride1
                   vz
                   (+ offz iz)
                   stride-z)))
           (set-box! out0
                     (tp-scalarize (flat s0 g0 0)))
           (set-box! out1
                     (tp-scalarize (flat s1 g1 0)))))))))

;; TODO: Create a lazy-apply-2 that does this more generally
(define tp-d-ext2^
  (λ (fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z)
    (let* ((out0 (box 'uncalculated))
           (out1 (box 'uncalculated)))
      (values
       (tpromise (tcomp-ext2-∇ fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 0)
                 (tp-shape tp-t0))
       (tpromise (tcomp-ext2-∇ fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 1)
                 (tp-shape tp-t1))))))

(define ensure-tpromise
  (λ (v)
    (cond
      ((scalar? v) (tpromise (ensure-flat v) '()))
      (else v))))


(define flat-gradient-maker2
  (λ (f m n)
    (cond
      ((and (zero? m) (zero? n))
       (λ (g0
           g1
           v0 i0 stride0
           v1 i1 stride1
           vz iz stride-z)
         (let ((z (vref vz iz))
               (a (vref v0 i0))
               (b (vref v1 i1)))
           (let-values (((da db) (f a b z)))
             (vset! g0 i0
                    (+ (vref g0 i0) da))
             (vset! g1 i1
                    (+ (vref g1 i1) db))))))
      (else f))))

(define flat-gradient-maker1
  (λ (f0 m)
    (cond
      ((zero? m)
       (λ (g0 v0 i0 stride0 vz iz stride-z)
         (let ((z (vref vz iz))
               (a (vref v0 i0)))
           (vset! g0 i0 (+ (vref g0 i0)
                           (f0 a z))))))
      (else f0))))

(define tp-rank
  (λ (tp)
    (flat:len (tp-shape tp))))

(define tp-reshape
  (λ (s tp)
    (cond
      ((= (flat:size-of s) (flat:size-of (tpromise-shape tp)))
       (tpromise (tcomp-reshape s tp) s))
      (else (error "Cannot reshape ~a to ~a~%" (tpromise-shape tp) s)))))

(define tensor?
  (lambda (tp)
    (or (tpromise? tp) (flat? tp) (scalar? tp))))

(define force*1
  (λ (t f)
    (f (tp-force t))))

(define force*2
  (λ (ts f)
    (let-values (((t1 t2) (ts)))
      (f (tp-force t1) (tp-force t2)))))

(include "test/test-0-lazy.rkt")

(provide start-vector-manager vector-manager-report)

(provide (rename-out
          (flat:len len)
          (flat:ref ref)
          (flat:refr refr)))
(provide tensor
         tp-force
         tpromise?
         (rename-out
          (tp-scalarize scalarize)
          (tp-tref tref)
          (tp-tlen tlen)
          (list->tpromise list->tensor)
          (build-tpromise build-tensor)
          (tp-trefs trefs)))

(provide (rename-out
          (tp-ext1-ρ ext1-ρ)
          (tp-ext2-ρ ext2-ρ)
          (tp-ext1-∇ ext1-∇)
          (tp-ext2-∇ ext2-∇)))

;; These will get overriden by duals
(provide tensor?)
(provide (rename-out
          (tp-rank rank)
          (tp-shape shape)
          (tp-reshape reshape)
          (flat:size-of size-of)))

(provide force*1 force*2)
