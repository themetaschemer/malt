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
    (let ((flat-f (flat-ext1-ρ (flat-function-maker1 f m) m shape-fn)))
      (λ (t)
        (cond
          ((number? t) (f t))
          (else (scalarize (flat-f t))))))))

(define ext1-∇
  (λ (f m [shape-fn scalar-shape])
    (let ((flat-f (flat-ext1-∇ (flat-gradient-maker1 f m) m shape-fn)))
      (λ (t z)
        (cond
         ((number? t) (f t z))
         ((number? z) (scalarize (flat-f t (flat '() (vector z) 0))))
         (else
          (scalarize (flat-f t z))))))))

(define flat-function-maker1
  (λ (f m)
    (cond
      ((zero? m)
       (λ (v0 i0 stride0 v-out i-out stride-out)
         (vset! v-out i-out (f (vref v0 i0)))))
      (else f))))

(define flat-gradient-maker1
  (λ (f0 m)
    (cond
     ((zero? m)
      (λ (g0 v0 i0 stride0 vz iz stride-z)
        (let ((z (vref vz iz))
              (a (vref v0 i0)))
          (vset! g0 i0 (+ (vref g0 i0) (f0 a z))))))
     (else f0))))

;;—————————————————–—————————————————–—————————————————–
;; Binary Pointwise extension
;;—————————————————–—————————————————–—————————————————–

(define ext2-ρ
  (λ (f m n [shape-fn scalar-shape])
    (let ((flat-f (flat-ext2-ρ (flat-function-maker2 f m n) m n shape-fn)))
      (λ (t u)
        (cond
          ((and (number? t) (number? u)) (f t u))
          ((number? t) (scalarize (flat-f (flat '() (vector t) 0) u)))
          ((number? u) (scalarize (flat-f t (flat '() (vector u) 0))))
          (else (scalarize (flat-f t u))))))))

(define ext2-∇
  (λ (f m n [shape-fn scalar-shape])
    (let ((flat-f
           (let ((f (flat-ext2-∇ (flat-gradient-maker2 f m n) m n shape-fn)))
             (λ (t u z)
               (let-values (((dt du) (f t u z)))
                 (values (scalarize dt) (scalarize du)))))))
      (λ (t u z)
        (cond
          ((and (number? t) (number? u)) (f t u z))
          ((number? t) (flat-f (flat '() (vector t) 0) u z))
          ((number? u) (flat-f t (flat '() (vector u) 0) z))
          ((number? z) (flat-f t u (flat '() (vector z) 0)))
          (else (flat-f t u z)))))))

(define flat-function-maker2
  (λ (f m n)
    (cond
      ((and (zero? m) (zero? n))
       (λ (v0 i0 stride0 v1 i1 stride1 v-out i-out stride-out)
         (vset! v-out i-out (f (vref v0 i0) (vref v1 i1)))))
      (else
       f))))

(define flat-gradient-maker2
  (λ (f m n)
    (cond
      ((and (zero? m) (zero? n))
       (λ (g0 g1 v0 i0 stride0 v1 i1 stride1 vz iz stride-z)
         (let ((z (vref vz iz))
               (a (vref v0 i0))
               (b (vref v1 i1)))
           (let-values (((da db) (f a b z)))
             (vset! g0 i0 (+ (vref g0 i0) da))
             (vset! g1 i1 (+ (vref g1 i1) db))))))
      (else f))))


(define scalar-shape
  (λ (s0 [s1 '()]) '()))


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

(define scalarize
  (λ (t)
    (cond
      ((null? (flat-shape t)) (vref (flat-store t) 0))
      (else t))))

(define min-shape
  (λ (min-rank in-shape)
    (drop in-shape (- (length in-shape) min-rank))))

(define merge-shapes
  (λ (in-shape min-rank out-f-shape)
    (append (take in-shape (- (length in-shape) min-rank))
            out-f-shape)))

(define flat-ext1-ρ
  (λ (f min-rank shape-fn [context 'ext1])
    (λ (t0 [out-vector #f] [out-offset 0])
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
             (v-out (or out-vector (new-vec size-out 0.0 context))))
        (for ([i-out (in-range out-offset (+ out-offset size-out) stride-out)]
              [i0 (in-range off0 (+ off0 size0) stride0)])
          (f v0 i0 stride0 v-out i-out stride-out))
         (flat s-out v-out out-offset)))))

(define flat-ext1-∇
  (λ (fᵈ min-rank shape-fn [context 'd-ext1])
    ;; z has the same shape as the output
    (λ (t0 z)
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

             (g0 (new-vec size0 0.0 context)))
        (for ([iz (in-range 0 size-z stride-z)]
              [i0 (in-range off0 (+ off0 size0) stride0)])
          (fᵈ g0 v0 i0 stride0 vz iz stride-z))
        (flat s0 g0 0)))))

(define flat-ext2-ρ
  (λ (f r0 r1 shape-fn)
    (λ (t0 t1 [out-vector #f] [out-offset 0])
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
            (let ((out-v (or out-vector (make-vector size-out 0.0))))
              (for ([out-i (in-range 0 size-out stride-out)])
                (let-values (((i0 i1)
                              (idxs strides out-i off0 off1)))
                  (f v0 i0 stride0 v1 i1 stride1 out-v (+ out-offset out-i) stride-out)))
              (flat s-out out-v out-offset))))))))

(define flat-ext2-∇
  (λ (fᵈ r0 r1 shape-fn [context 'd-ext2])
    (λ (t0 t1 z)
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
            (let ((g0 (new-vec (size-of s0) 0.0 context))
                  (g1 (new-vec (size-of s1) 0.0 context)))
              (for ([iz (in-range 0 size-z stride-z)])
                (let-values (((i0 i1)
                              (idxs strides iz off0 off1)))
                  (fᵈ g0 g1 v0 i0 stride0 v1 i1 stride1 vz (+ offz iz) stride-z)))
              (values (flat s0 g0 0)
                      (flat s1 g1 0)))))))))


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

(include "test/test-D-extend.rkt")

(provide ext1-ρ ext1-∇ ext2-ρ ext2-∇)
