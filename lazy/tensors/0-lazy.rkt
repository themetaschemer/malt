#lang racket
(require "../../flat-tensors/ext-impl.rkt")
(require (prefix-in flat: "../../flat-tensors/tensors.rkt"))

(require "c0-ast.rkt")
(require (only-in "c1-racket-runtime.rkt" ext2-∇-result))

#|
Questions:

* How do I create a preallocated function example for ext2-∇? Also, how do the preallocated functions work?
A. Here are what the formal parameters of a binary preallocated ∇-function mean:

    - g0, g1: These are the empty gradient tensor stores which need to be filled
with the gradients of the corresponding ρ-function w.r.t. the first and second
arguments respectively.

    - t, it, st: the store, beginning offset and total size of the flat
representation of the first tensor argument respectively which we will need to loop through
the scalar elements.

    - u, iu, su: the store, beginning offset and total size of the flat
representation of the second tensor argument respectively which we will need to
loop through the scalar elements.

    - z, iz, sz: the store, beginning offset and total size of the flat
representation of the accumulator respectively which we will need to
loop through the scalar elements.

   Here are the invariants of the formal parameters:

    - The flat tensors corresponding to the stores g0 and g1 have the same shape
as the first and second input tensors respectively

    - The flat tensor corresponding to the store z has the same shape as the
result of invoking the corresponding ρ-function with the two tensor arguments

* Here are a few problems which need to be addressed while checking compiler
invariants after compiling the expression "(((l2-loss plane) r2d1 (tensor 1.0
1.0)) plane-theta-0)" from the test-C-loss.rkt file:

    - The env contains flat tensors with the shape '() i.e. they scalars in
disguise

    - Somehow copies of a flat tensor are being added to the env rather than the
instructions refering to the same gensym variable

|#

#;
(: scalar? (-> Any Boolean))
(define scalar? number?)

#;
(: tensor (case-> (-> tpromise * tpromise)
                  (-> Number * tpromise)))
(define tensor
  (λ args
    (list->tpromise args)))
#;
(: tensor-inner-flat (-> (Listof (U tpromise Number))
                       (U flat tcomp-list->tensor)))
(define tensor-inner-flat
  (λ (lst)
    (cond
     [(andmap number? lst) (apply flat:tensor lst)]
     [else (tcomp-list->tensor lst)])))

#;
(: ensure-shape (-> (U (Listof tpromise) (Listof Number)) Void))
(define ensure-shape
  (λ (args)
    (when (null? args)
      (error 'tensor "Tensors cannot be empty"))
    (let ((checked-shape
           (λ (x) (if (tpromise? x)
                      (tpromise-shape x)
                      '())))
          (scalar-like?
           (λ (x)
             (or (number? x)
                 (and (tpromise? x)
                      (null? (tpromise-shape x)))))))
      (unless (and (not (null? args))
                   (cond
                     ((scalar-like? (car args))
                      (andmap scalar-like? (cdr args)))
                     ((tpromise? (car args))
                      (let ((s (checked-shape (car args))))
                        (andmap (λ (t)
                                  (and (tpromise? t)
                                       (equal? (checked-shape t) s)))
                                (cdr args))))
                     (else #f)))
        (error 'tensor
               "Cannot construct a tensor out of these elements: ~a~%"
               args)))))

(define list->tpromise
  (λ (lst)
    (ensure-shape lst)
    (let ((inner-flat (tensor-inner-flat lst)))
      (cond
        ((flat? inner-flat)
         (tpromise inner-flat (flat-shape inner-flat)))
        (else
         (let* ((inner-shape (tp-shape (car lst)))
                (outer (length lst))
                (new-shape (cons outer inner-shape)))
           (tpromise inner-flat new-shape)))))))

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
       (tpromise (tcomp-tref tp i)
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

(define build-tpromise
  (λ (s f)
    (tpromise (tcomp-build-tensor s f) s)))

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
       (tpromise (tcomp-trefs tp b)
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
          (tcomp-ext1-ρ f m shape-fn tp)
          (merge-shapes
           (tp-shape tp)
           m
           (shape-fn
            (min-shape m (tp-shape tp)))))]
        [else
         (let* ((in-shape (tpromise-shape tp))
                (base-shape (min-shape m in-shape))
                (out-shape (shape-fn base-shape))
                (flat-f (functional->preallocated-1-ρ f base-shape out-shape)))
           (tpromise
            (tcomp-ext1-ρ flat-f m shape-fn tp)
            (merge-shapes
             (tp-shape tp)
             m
             (shape-fn
              (min-shape m (tp-shape tp))))))]))))

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
         (let* ((s0 (tp-shape tp-t))
                (s1 (tp-shape tp-u))
                (sf0 (min-shape m s0))
                (sf1 (min-shape n s1))
                (sf-out (shape-fn sf0 sf1)))
           (tpromise
            (tcomp-ext2-ρ (ensure-tpromise tp-t)
                          (ensure-tpromise tp-u)
                          f m n shape-fn)
            (ext2-shapes s0 s1 m n sf-out
                         (λ (s-out . _) s-out))))]
        [else
         (let* ((s0 (tp-shape tp-t))
                (s1 (tp-shape tp-u))
                (sf0 (min-shape m s0))
                (sf1 (min-shape n s1))
                (sf-out (shape-fn sf0 sf1))
                (t-shape (min-shape m s0))
                (u-shape (min-shape n s1))
                (out-shape (shape-fn t-shape u-shape))
                (flat-f (functional->preallocated-2-ρ
                         f
                         t-shape
                         u-shape
                         out-shape)))
           (tpromise
            (tcomp-ext2-ρ (ensure-tpromise tp-t)
                          (ensure-tpromise tp-u)
                          flat-f m n shape-fn)
            (ext2-shapes s0 s1 m n sf-out
                         (λ (s-out . _) s-out))))]))))

(define scalar-shape
  (λ (s0 [s1 '()]) '()))

(define tp-ext1-∇
  (λ (f m [shape-fn scalar-shape])
    (λ (tp zp)
      (cond
        ((number? tp) (f tp zp))
        ((flat:expects-preallocated? f)
         (tpromise
          (tcomp-ext1-∇ tp (ensure-tpromise zp) f m shape-fn)
          (tp-shape tp)))
        (else
         (let* ((in-shape (tpromise-shape tp))
                (base-shape (min-shape m in-shape))
                (out-shape (shape-fn base-shape))
                (flat-f (functional->preallocated-1-∇ f base-shape out-shape)))
           (tpromise
            (tcomp-ext1-∇ tp (ensure-tpromise zp) flat-f m shape-fn)
            (tp-shape tp))))))))

(define tp-ext2-∇
  (λ (f m n [shape-fn scalar-shape])
    (let ((tp-f
           (λ (f tp-t tp-u tp-z)
             (tp-d-ext2^ f m n shape-fn
                         tp-t tp-u tp-z))))
      (λ (tp-t tp-u tp-z)
        (cond
          ((flat:expects-preallocated? f)
           (tp-f f
                 (ensure-tpromise tp-t)
                 (ensure-tpromise tp-u)
                 (ensure-tpromise tp-z)))
          [else (let* ((t-shape (min-shape m (tp-shape tp-t)))
                       (u-shape (min-shape n (tp-shape tp-u)))
                       (out-shape (shape-fn t-shape u-shape))
                       (flat-f (functional->preallocated-2-∇
                                f t-shape u-shape out-shape)))
                  (tp-f flat-f
                        (ensure-tpromise tp-t)
                        (ensure-tpromise tp-u)
                        (ensure-tpromise tp-z)))])))))


(define tp-d-ext2^
  (λ (fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z)
    (let* ((out0 (ext2-∇-result 'uncalculated))
           (out1 (ext2-∇-result 'uncalculated)))
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

(define tp-rank
  (λ (tp)
    (flat:len (tp-shape tp))))

(define tp-reshape
  (λ (s tp)
    (cond
      ((= (flat:size-of s) (flat:size-of (tpromise-shape tp)))
       (tpromise (tcomp-reshape s tp) s))
      (else (error 'shape-error "Cannot reshape ~a to ~a~%" (tpromise-shape tp) s)))))

(define tensor?
  (lambda (tp)
    (or (tpromise? tp) (flat? tp) (scalar? tp))))

(include "test/test-0-lazy.rkt")

(provide start-vector-manager vector-manager-report)

(provide (rename-out
          (flat:len len)
          (flat:ref ref)
          (flat:refr refr)))
(provide tensor
         tpromise?
         (rename-out
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
