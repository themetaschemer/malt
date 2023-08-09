#lang racket

(require "0-vectors.ss")
(require "1-flats.ss")
(require "B-tensor-basics.ss")
(require "C-tensor-ops.ss")

;;—————————————————–—————————————————–—————————————————–
;; Unary Pointwise extension
;;—————————————————–—————————————————–—————————————————–

(define ext1-ρ
  (λ (f m [shape-fn scalar-shape])
    (λ (t)
      (cond
        ((number? t) (f t))
        ((expects-preallocated? f)
         (scalarize
          (flat-ext1-ρ f m shape-fn t)))
        (else
         (let* ((in-shape (flat-shape t))
                (base-shape (min-shape m in-shape))
                (out-shape (shape-fn base-shape))
                (flat-f (functional->preallocated-1-ρ f base-shape out-shape)))
           (scalarize
            (flat-ext1-ρ flat-f m shape-fn t))))))))

(define ext1-∇
  (λ (f m [shape-fn scalar-shape])
    (λ (t z)
      (cond
        ((number? t) (f t z))
        ((expects-preallocated? f)
         (scalarize (flat-ext1-∇ f m shape-fn t (ensure-flat z))))
        (else
         (let* ((in-shape (flat-shape t))
                (base-shape (min-shape m in-shape))
                (out-shape (shape-fn base-shape))
                (flat-f (functional->preallocated-1-∇ f base-shape out-shape)))
         (scalarize (flat-ext1-∇ flat-f m shape-fn t (ensure-flat z)))))))))

(define functional->preallocated-1-ρ
  (λ (f base-shape out-shape)
    (λ (v0 i0 stride0 v-out i-out stride-out)
      (set-prealloc-ρ! v-out i-out out-shape
        (f (arg-value base-shape v0 i0))))))

(define functional->preallocated-1-∇
  (λ (f base-shape out-shape)
    (λ (g0 v0 i0 stride0 vz iz stride-z)
      (let ((z (arg-value out-shape vz iz))
            (a (arg-value base-shape v0 i0)))
        (set-prealloc-∇! g0 i0 base-shape (f a z))))))

(define set-prealloc-ρ!
  (λ (v-out i-out out-shape a)
    (cond
      ((null? out-shape) (vset! v-out i-out a))
      (else (v-copy-flat! v-out i-out a)))))

(define set-prealloc-∇!
  (λ (v-out i-out out-shape a)
    (cond
      ((null? out-shape) (vset! v-out i-out (+ (vref v-out i-out) a)))
      (else (v-add-flat! v-out i-out a)))))

(define arg-value
  (λ (v-shape v i)
    (cond
      ((null? v-shape) (vref v i))
      (else (flat v-shape v i)))))


(define invoke-functional-∇
  (λ (f base-shape v0 i0)
    (cond
      ((null? base-shape) (f (vref v0 i0)))
      (else (f (flat base-shape v0 i0))))))

;;—————————————————–—————————————————–—————————————————–
;; Binary Pointwise extension
;;—————————————————–—————————————————–—————————————————–

(define ext2-ρ
  (λ (f m n [shape-fn scalar-shape])
    (λ (t u)
      (cond
        ((and (number? t) (number? u)) (f t u))
        ((expects-preallocated? f)
         (scalarize
          (flat-ext2-ρ f m n shape-fn t u)))
        ((number? t)
         (let* ((t-shape '())
                (u-shape (min-shape n (flat-shape u)))
                (out-shape (shape-fn t-shape u-shape))
                (flat-f (functional->preallocated-2-ρ f t-shape u-shape out-shape)))
           (scalarize
            (flat-ext2-ρ flat-f m n shape-fn (ensure-flat t) u))))
        ((number? u)
         (let* ((t-shape (min-shape m (flat-shape t)))
                (u-shape '())
                (out-shape (shape-fn t-shape u-shape))
                (flat-f (functional->preallocated-2-ρ f t-shape u-shape out-shape)))
           (scalarize
            (flat-ext2-ρ flat-f m n shape-fn t (ensure-flat u)))))
        (else
         (let* ((t-shape (min-shape m (flat-shape t)))
                (u-shape (min-shape n (flat-shape u)))
                (out-shape (shape-fn t-shape u-shape))
                (flat-f (functional->preallocated-2-ρ f t-shape u-shape out-shape)))
           (scalarize
            (flat-ext2-ρ flat-f m n shape-fn t u))))))))

(define ext2-∇
  (λ (f m n [shape-fn scalar-shape])
    (λ (t u z)
      (let ((invoke-flat-ext2-∇
             (λ (f m n shape-fn t u z)
               (let-values (((da db) (flat-ext2-∇ f m n shape-fn t u z)))
                 (values (scalarize da) (scalarize db))))))
      (cond
        ((and (number? t) (number? u)) (f t u z))
        ((expects-preallocated? f)
         (invoke-flat-ext2-∇ f m n shape-fn t u z))
        ((number? t)
         (let* ((t-shape '())
                (u-shape (min-shape n (flat-shape u)))
                (out-shape (shape-fn t-shape u-shape))
                (flat-f (functional->preallocated-2-∇ f t-shape u-shape out-shape)))
           (invoke-flat-ext2-∇ flat-f m n shape-fn (ensure-flat t) u z)))
        ((number? u)
         (let* ((t-shape (min-shape m (flat-shape t)))
                (u-shape '())
                (out-shape (shape-fn t-shape u-shape))
                (flat-f (functional->preallocated-2-∇ f t-shape u-shape out-shape)))
           (invoke-flat-ext2-∇ flat-f m n shape-fn t (ensure-flat u) z)))
        (else
         (let* ((t-shape (min-shape m (flat-shape t)))
                (u-shape (min-shape n (flat-shape u)))
                (out-shape (shape-fn t-shape u-shape))
                (flat-f (functional->preallocated-2-∇ f t-shape u-shape out-shape)))
           (invoke-flat-ext2-∇ flat-f m n shape-fn t u z))))))))

(define functional->preallocated-2-ρ
  (λ (f t-shape u-shape out-shape)
    (λ (v0 i0 stride0 v1 i1 stride1 v-out i-out stride-out)
      (set-prealloc-ρ! v-out i-out out-shape
        (f (arg-value t-shape v0 i0)
           (arg-value u-shape v1 i1))))))

(define functional->preallocated-2-∇
  (λ (f t-shape u-shape out-shape)
    (λ (g0 g1 v0 i0 stride0 v1 i1 stride1 vz iz stride-z)
      (let ((z (arg-value out-shape vz iz))
            (a (arg-value t-shape v0 i0))
            (b (arg-value u-shape v1 i1)))
        (let-values (((da db) (f a b z)))
          (set-prealloc-∇! g0 i0 t-shape da)
          (set-prealloc-∇! g1 i1 u-shape db))))))

(define idxs
  (λ (strides out-i i0 i1)
    (for/fold ([i0 i0]
               [i1 i1]
               [x out-i] #:result (values i0 i1))
              ([stride strides])
      (let ((idx (quotient x (vector-ref stride 0)))
            (next-x (remainder x (vector-ref stride 0))))
        (values (+ i0 (* idx (vector-ref stride 1)))
                (+ i1 (* idx (vector-ref stride 2)))
                next-x)))))

(define merge-shapes
  (λ (in-shape min-rank out-f-shape)
    (append (take in-shape (- (length in-shape) min-rank))
            out-f-shape)))

(define flat-ext1-ρ
  (λ (f min-rank shape-fn t0)
    (let* ((s0 (flat-shape t0))
           (v0 (flat-store t0))
           (off0 (flat-offset t0))
           (sf0 (min-shape min-rank s0))
           (stride0 (size-of sf0))
           (size0 (size-of s0))

           (sf-out (shape-fn sf0))
           (stride-out (size-of sf-out))
           (s-out (merge-shapes s0 min-rank sf-out))
           (size-out (size-of s-out))
           (v-out (new-vec size-out 0.0)))
      (for ([i-out (in-range 0 size-out stride-out)]
            [i0 (in-range off0 (+ off0 size0) stride0)])
        (f v0 i0 stride0 v-out i-out stride-out))
      (flat s-out v-out 0))))

(define flat-ext1-∇
  (λ (fᵈ min-rank shape-fn t0 z)
    ;; z has the same shape as the output
    (let* ((s0 (flat-shape t0))
           (v0 (flat-store t0))
           (off0 (flat-offset t0))
           (sf0 (min-shape min-rank s0))
           (stride0 (size-of sf0))
           (size0 (size-of s0))

           (sz (flat-shape z))
           (size-z (size-of sz))
           (sf-z (shape-fn sf0))
           (stride-z (size-of sf-z))
           (vz (flat-store z))

           (g0 (new-vec size0 0.0)))
      (for ([iz (in-range 0 size-z stride-z)]
            [i0 (in-range off0 (+ off0 size0) stride0)])
        (fᵈ g0 v0 i0 stride0 vz iz stride-z))
      (flat s0 g0 0))))

(define flat-ext2-ρ
  (λ (f r0 r1 shape-fn t0 t1)
    (let* ((s0 (flat-shape t0))
           (v0 (flat-store t0))
           (off0 (flat-offset t0))
           (sf0 (min-shape r0 s0))

           (s1 (flat-shape t1))
           (v1 (flat-store t1))
           (off1 (flat-offset t1))
           (sf1 (min-shape r1 s1))

           (sf-out (shape-fn sf0 sf1))
           (stride0 (size-of sf0))
           (stride1 (size-of sf1))
           (stride-out (size-of sf-out)))
      (ext2-shapes s0 s1 r0 r1 sf-out
        (λ (s-out size-out q0 q1 strides)
          (let ((out-v (make-vector size-out 0.0)))
            (for ([out-i (in-range 0 size-out stride-out)])
              (let-values (((i0 i1)
                            (idxs strides out-i off0 off1)))
                (f v0 i0 stride0 v1 i1 stride1 out-v (+ 0 out-i) stride-out)))
            (flat s-out out-v 0)))))))

(define flat-ext2-∇
  (λ (fᵈ r0 r1 shape-fn t0 t1 z)
    (let* ((s0 (flat-shape t0))
           (v0 (flat-store t0))
           (off0 (flat-offset t0))
           (sf0 (min-shape r0 s0))
           (stride0 (size-of sf0))

           (s1 (flat-shape t1))
           (v1 (flat-store t1))
           (off1 (flat-offset t1))
           (sf1 (min-shape r1 s1))
           (stride1 (size-of sf1))

           (sf-z (shape-fn sf0 sf1))
           (stride-z (size-of sf-z))
           (vz (flat-store z))
           (offz (flat-offset z)))
      (ext2-shapes s0 s1 r0 r1 sf-z
        (λ (sz size-z q0 q1 strides)
          (let ((g0 (new-vec (size-of s0) 0.0))
                (g1 (new-vec (size-of s1) 0.0)))
            (for ([iz (in-range 0 size-z stride-z)])
              (let-values (((i0 i1)
                            (idxs strides iz off0 off1)))
                (fᵈ g0 g1 v0 i0 stride0 v1 i1 stride1 vz (+ offz iz) stride-z)))
            (values (flat s0 g0 0)
                    (flat s1 g1 0))))))))

(define ext2-shapes
  (λ (s0 s1 r0 r1 sf-out k)
    (let ((l0 (length s0))
          (l1 (length s1)))
      (cond
        ((and (= r0 l0) (= r1 l1))
           (k sf-out
              (size-of sf-out)
              (size-of s0)
              (size-of s1)
              '()))

        ((= r0 l0)
         (ext2-shapes s0 (cdr s1) r0 r1 sf-out
           (desc-right (car s1) k)))

        ((= r1 l1)
         (ext2-shapes (cdr s0) s1 r0 r1 sf-out
           (desc-left (car s0) k)))

        ((and (not (null? s0))
              (not (null? s1))
              (= (car s0) (car s1)))
         (ext2-shapes (cdr s0) (cdr s1) r0 r1 sf-out
           (desc-both (car s0) k)))

        ((> l1 l0)
         (ext2-shapes s0 (cdr s1) r0 r1 sf-out
           (desc-right (car s1) k)))

        ((> l0 l1)
         (ext2-shapes (cdr s0) s1 r0 r1 sf-out
           (desc-left (car s0) k)))

        (else (error 'ext
               "Shapes are incompatible for ext2: ~a, and ~a for min ranks ~a, and ~a~%"
               s0 s1 r0 r1))))))

(define desc-both
  (λ (d k)
    (λ (s-out qout q0 q1 strides)
      (k (cons d s-out)
         (* qout d)
         (* q0 d)
         (* q1 d)
         (cons (vector qout q0 q1) strides)))))

(define desc-left
  (λ (d k)
    (λ (s-out qout q0 q1 strides)
      (k (cons d s-out)
         (* qout d)
         (* q0 d)
         q1
         (cons (vector qout q0 0) strides)))))

(define desc-right
  (λ (d k)
    (λ (s-out qout q0 q1 strides)
      (k (cons d s-out)
         (* qout d)
         q0
         (* q1 d)
         (cons (vector qout 0 q1) strides)))))

(define v-copy-flat!
  (λ (vg ig a)
    ;; copy elements from a to vg
    (let ((va (flat-store a))
          (a-offset (flat-offset a))
          (a-stride (size-of (flat-shape a))))
      (for ([i (in-range 0 a-stride)])
        (vset! vg (+ ig i)
               (vref va (+ a-offset i)))))))

(define v-add-flat!
  (λ (vg ig a)
    ;; copy elements to a to vg while adding them to vg
    (let ((va (flat-store a))
          (a-offset (flat-offset a))
          (a-stride (size-of (flat-shape a))))
      (for ([i (in-range 0 a-stride)])
        (vset! vg (+ ig i)
               (+ (vref vg (+ ig i))
                  (vref va (+ a-offset i))))))))

(define expects-preallocated?
  (λ (f)
    (let ((a (procedure-arity f)))
      (and (integer? a)
        (>= a 6)))))

(define ensure-flat
  (λ (z)
    (cond
      ((number? z)
       (flat '() (vector z) 0))
      (else z))))

(define scalarize
  (λ (t)
    (cond
      ((null? (flat-shape t)) (vref (flat-store t) 0))
      (else t))))

(define min-shape
  (λ (min-rank in-shape)
    (drop in-shape (- (length in-shape) min-rank))))

(define scalar-shape
  (λ (s0 [s1 '()]) '()))

(include "test/test-D-extend.rkt")

(provide ext1-ρ ext1-∇ ext2-ρ ext2-∇ expects-preallocated?
         functional->preallocated-1-ρ functional->preallocated-1-∇
         functional->preallocated-2-ρ functional->preallocated-2-∇
         merge-shapes min-shape ext2-shapes idxs
         flat-ext1-∇ flat-ext1-ρ flat-ext2-ρ scalarize)
