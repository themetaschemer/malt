#lang racket
(require "../../flat-tensors/ext-impl.rkt")
(require (prefix-in flat: "../../flat-tensors/tensors.rkt"))

;; tensor computations
(struct tcomp ())
;; TODO: figure out if removing tcom-tensor is a good idea
(struct tcomp-tensor tcomp (t-shape t-flat) #:transparent)
(struct tcomp-list->tpromise-list tcomp (lst) #:transparent)
(struct tcomp-tp-map tcomp (f tp) #:transparent)
(struct tcomp-build-tpromise tcomp (s f) #:transparent)
(struct tcomp-tp-trefs tcomp (forced b) #:transparent)
(struct tcomp-ext2-∇ tcomp (b forcer) #:transparent)
(struct tcomp-ext1-∇ tcomp (tp zp flat-f) #:transparent)
(struct tcomp-ext2-ρ tcomp (tp-t tp-u flat-f) #:transparent)
(struct tcomp-ext1-ρ tcomp (tp flat-f) #:transparent)
(struct tcomp-reshape tcomp (s tp) #:transparent)

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

(define scalar? number?)

(define tensor
  (λ args
    (ensure-shape args)
    (let ([inner-flat (tensor-inner-flat args)])
      (tpromise inner-flat (flat:shape inner-flat)))))

(define tensor-inner-flat
  (λ (args)
    (cond
     [(number? (car args)) (apply flat:tensor args)]
     [else (merge-flats (map tp-force args))])))

(define ensure-shape
  (λ (args)
    (unless (and (not (null? args))
                 (cond
                   ((number? (car args))
                    (andmap number? (cdr args)))
                   ((tpromise? (car args))
                    (let ((s (tp-shape (car args))))
                      (andmap (λ (t)
                                (and (tpromise? t)
                                     (equal? (tp-shape t) s)))
                              (cdr args))))
                   (else #f)))
      (error 'tensor
             "Mismatched shapes: ~a~%"
             args))))

(define ensure-flat
  (λ (v)
    (cond
      ((scalar? v) (flat '() (vec v) 0))
      (else v))))

;(-> tpromise (U flat scalar))
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

(define tcomp-force
  (λ (tc)
    (match tc
      #;[(tcomp-tensor t-shape t-flat)
         (flat t-shape t-flat 0)]
      [(tcomp-list->tpromise-list lst)
       (flat:list->tensor
        (map (λ (l) (tp-force l #f)) lst))]
      [(tcomp-tp-map f tp)
       (let* ([flat-vec (tp-force tp #f)]
              [store (flat-store flat-vec)]
              [shape (flat-shape flat-vec)]
              [offset (flat-offset flat-vec)])
         (flat shape (vector-map f store)
               offset))]
      [(tcomp-build-tpromise s f)
       (flat:build-tensor s f)]
      [(tcomp-tp-trefs forced b)
       (flat:trefs forced b)]
      [(tcomp-ext2-∇ b forcer)
       (let ([v (unbox b)])
         (cond
           ((eqv? v 'uncalculated)
            (forcer)
            (unbox b))
           (else v)))]
      [(tcomp-ext1-∇ tp zp flat-f)
       (let ([t (tp-force tp #f)]
             [z (tp-force zp #f)])
         (scalarize (flat-f (ensure-flat t) (ensure-flat z))))]
      [(tcomp-ext2-ρ tp-t tp-u flat-f)
       (let ([t (tp-force tp-t #f)]
             [u (tp-force tp-u #f)])
         (scalarize (flat-f (ensure-flat t) (ensure-flat u))))]
      [(tcomp-ext1-ρ tp flat-f)
       (let ([t (tp-force tp #f)])
         (let ([res (scalarize (flat-f t))])
           res))]
      [(tcomp-reshape s tp)
       (let ([t (tp-force tp #f)])
         (flat s (flat-store t) (flat-offset t)))])))

(define tp-force-ref
  (λ (tp i)
    (flat:tref (tp-force tp) i)))

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
      [(and (bounded-idx*? tp (list i))
            (flat? (tp-force-ref tp i)))
       (tpromise (tp-force-ref tp i)
                 (flat-shape (tp-force-ref tp i)))]
      [(bounded-idx*? tp (list i))
       (tp-force-ref tp i)]
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

(define tp-tmap
  (λ (f tp)
    (struct-copy
     tpromise tp
     (tensor
      (tcomp-tp-map f tp)))))

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
       (let ([forced (tp-force tp)])
         (tpromise (tcomp-tp-trefs forced b)
                   `(,(length b)
                     . ,(cdr (flat-shape forced)))))])))

(define tp-ext1-ρ
  (λ (f
      m
      [shape-fn scalar-shape]
      [context 'lazy-ext1])
    (let ((flat-f
           (flat-ext1-ρ (flat-function-maker1 f m)
                 m shape-fn context)))
      (λ (tp)
        (cond
          [(scalar? tp) (f tp)]
          [(and (tpromise? tp)
                (null? (tpromise-shape tp)))
           (f (tp-force tp))]
          [else
           (tpromise
            (tcomp-ext1-ρ tp flat-f)
            (merge-shapes
             (tpromise-shape tp)
             m
             (shape-fn
              (min-shape m (tpromise-shape tp)))))])))))

(define tp-ext2-ρ
  (λ (f
      m
      n
      [shape-fn scalar-shape]
      [context 'raw-ext2])
    (let ((flat-f
           (flat-ext2-ρ (flat-function-maker2 f m n)
                 m n shape-fn #;context)))
      (λ (tp-t tp-u)
        (cond
          ((and (number? tp-t) (number? tp-u))
           (f tp-t tp-u))
          [(and (tpromise? tp-t) (tpromise? tp-u)
                (null? (tpromise-shape tp-t))
                (null? (tpromise-shape tp-u)))
           (f (tp-force tp-t) (tp-force tp-u))]
          [else
           (let* ([s0 (tp-shape tp-t)]
                  [s1 (tp-shape tp-u)]
                  [sf0 (min-shape m s0)]
                  [sf1 (min-shape n s1)]
                  [sf-out (shape-fn sf0 sf1)])
             (tpromise
              (tcomp-ext2-ρ tp-t tp-u flat-f)
              (ext2-shapes s0 s1 m n sf-out
                           (λ (s-out . _) s-out))))])))))

(define scalarize
  (λ (t)
    (cond
      ((null? (flat-shape t))
       (vref (flat-store t) 0))
      (else t))))

(define tp-scalarize
  (λ (tp)
    (cond
      [(and (tpromise? tp) (null? (tpromise-shape tp)))
       (scalarize (tp-force tp))]
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
  (λ (f
      m
      [shape-fn scalar-shape]
      [context 'lazy-d-ext1])
    (let ((flat-f
           (flat-ext1-∇ (flat-gradient-maker1 f m)
                   m shape-fn)))
      (λ (tp zp)
        (cond
          ((number? tp) (f tp zp))
          (else
           (tpromise
            (tcomp-ext1-∇ tp zp flat-f)
            (tpromise-shape tp))))))))

(define tp-d-ext2^
  (λ (fᵈ r0 r1 shape-fn [context 'lazy-flat-d-ext2])
    (λ (tp-t0 tp-t1 tp-z)
      (let* ((s0 (tpromise-shape tp-t0))
             (sf0 (min-shape r0 s0))
             (stride0 (flat:size-of sf0))

             (s1 (tpromise-shape tp-t1))
             (sf1 (min-shape r1 s1))
             (stride1 (flat:size-of sf1))

             (sf-z (shape-fn sf0 sf1))
             (stride-z (flat:size-of sf-z))

             (out0 (box 'uncalculated))
             (out1 (box 'uncalculated))
             (forcer
              (λ ()
                (let* ((f0 (ensure-flat (tp-force tp-t0)))
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
                                        0.0
                                        context))
                           (g1 (new-vec (flat:size-of
                                         s1)
                                        0.0
                                        context)))
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
                                 (flat s0 g0 0))
                       (set-box! out1
                                 (flat s1 g1 0)))))))))
        (values
         (tpromise (tcomp-ext2-∇ out0 forcer) s0)
         (tpromise (tcomp-ext2-∇ out1 forcer) s1))))))

(define ensure-tpromise
  (λ (v)
    (cond
      ((scalar? v) (tpromise (ensure-flat v) '()))
      (else v))))

(define tp-ext2-∇
  (λ (f
      m
      n
      [shape-fn scalar-shape]
      [context 'lazy-d-ext2])
    (let ((tp-f
           (let ((f (tp-d-ext2^
                     (flat-gradient-maker2 f m n)
                     m n shape-fn)))
             (λ (tp-t tp-u tp-z)
               (let-values (((tp-dt tp-du)
                             (f tp-t tp-u tp-z)))
                 (values (tp-scalarize tp-dt)
                         (tp-scalarize tp-du)))))))
      (λ (tp-t tp-u tp-z)
        (tp-f (ensure-tpromise tp-t)
              (ensure-tpromise tp-u)
              (ensure-tpromise tp-z))))))

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
    (or (tpromise? tp) (scalar? tp))))

(include "test/test-0-lazy.rkt")

(provide start-vector-manager vector-manager-report)

(provide (rename-out
          (flat:len len)
          (flat:ref ref)
          (flat:refr refr)))
(provide tensor
         tp-force
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
