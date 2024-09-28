#lang racket

;--------------------------------------------------------
; Memory management tools for vectors
;--------------------------------------------------------

(require "0-vectors.ss")
(require "1-flats.ss")

;--------------------------------------------------------
; Lists
;--------------------------------------------------------
(define ref list-ref)
(define refr drop)
(define len length)

(provide ref refr len)

;--------------------------------------------------------
; Tensor basics
;--------------------------------------------------------

(define tref
  (λ (t i)
    (cond
     ((= 1 (flat-rank t))
      (vref (flat-store t) (+ (flat-offset t) i)))
     (else
      (flat (cdr (flat-shape t))
            (flat-store t)
            (+ (flat-offset t) (* i (car (flat-strides t)))))))))

(define tlen
  (λ (t)
    (car (flat-shape t))))

(define flat-ref-idx
  (λ (v indices)
    (flat-ref-idx* (flat-offset v) (flat-strides v) indices)))

(define flat-ref-idx*
  (λ (current-idx strides indices)
    (cond
      ((null? indices) current-idx)
      (else
       (flat-ref-idx*
        (+ current-idx
           (* (car indices) (car strides)))
        (cdr strides)
        (cdr indices))))))

(define strides
  (λ (shape)
    (cond
      ((null? shape) '())
      (else (cons (size-of (cdr shape))
                  (strides (cdr shape)))))))

(define size-of
  (λ (shape)
    (product shape 1)))

(define product
  (λ (lst a)
    (cond
      ((null? lst) a)
      (else (product (cdr lst) (* (car lst) a))))))

(define list->tensor
  (λ (lst)
    (cond
      ((null? lst) (error 'list->flat-tensor "No elements found"))
      ((number? (car lst))
       (flat (list (length lst)) (list->vec lst) 0))
      (else
       (flat-tensor-from-list lst)))))

(define flat-tensor-from-list
  (λ (lst)
    (let* ([inner-shape (flat-shape (car lst))]
           [inner-size (size-of inner-shape)]
           [outer-shape (cons (length lst) inner-shape)]
           [size (size-of outer-shape)]
           [v (new-vec size 0.0 'from-list)])
      (for ([fl lst]
            [i (in-naturals 0)])
        (vcopy v (* i inner-size)
               (flat-store fl) (flat-offset fl)
               inner-size))
      (flat outer-shape v 0))))

(define tensor?
  (λ (t)
    (or (number? t)
        (flat? t))))

(define tensor
  (λ args
    (ensure-shape args)
    (cond
      ((number? (car args)) (flat (list (length args)) (list->vector args) 0))
      (else (merge-flats args)))))

(define merge-flats
  (λ (args)
    (let* ((inner-shape (flat-shape (car args)))
           (outer (length args))

           (new-shape (cons outer inner-shape))
           (stride (size-of inner-shape))

           (new-size (size-of new-shape))

           (v-out (new-vec new-size #f 'tensor)))
      (for ([i-out (in-range outer)]
            [arg args])
        (vcopy v-out (* i-out stride) (flat-store arg) (flat-offset arg) stride))
      (flat new-shape v-out 0))))

(define ensure-shape
  (λ (args)
    (when (null? args)
      (error 'tensor "Tensors cannot be empty"))
    (let ((checked-shape
           (λ (x) (if (flat? x)
                      (flat-shape x)
                      '()))))
      (unless (and (not (null? args))
                   (cond
                     ((number? (car args))
                      (andmap number? (cdr args)))
                     ((flat? (car args))
                      (let ((s (checked-shape (car args))))
                        (andmap (λ (t)
                                  (and (flat? t)
                                       (equal? (checked-shape t) s)))
                                (cdr args))))
                     (else #f)))
        (error 'tensor
               "Cannot construct a tensor out of these elements: ~a~%"
               args)))))

(define build-tensor
  (λ (shape f)
    (let* ((size (size-of shape))
           (v (new-vec size 0.0 'build-tensor))
           (strides (strides shape)))
      (fill-flat-tensor v shape strides f 0 '())
      (flat shape v 0))))

(define fill-flat-tensor
  (λ (dest shape strides f offset tidx)
    (cond
      ((null? (cdr shape))
       (for ([i (in-range 0 (car shape))])
         (vset! dest (+ offset i)
           (f (append tidx (list i))))))
      (else
       (let ((stride (car strides)))
         (for ([i (in-range 0 (car shape))])
           (fill-flat-tensor dest
             (cdr shape) (cdr strides) f
             (+ offset (* i stride)) (append tidx (list i)))))))))

(define trefs
  (λ (t b)
    (let* ([st (flat-shape t)]
           [est (cdr st)]
           [estride (size-of est)]
           [nshape (cons (length b) (cdr st))]
           [size-out (size-of nshape)]
           [v-out (new-vec size-out 0.0 'flat-refs)]
           [vt (flat-store t)])
      (for ([ib b]
            [i-out (in-range 0 size-out estride)])
        (vcopy v-out i-out vt (* ib estride) estride))
      (flat nshape v-out 0))))

(include "test/test-B-tensor-basics.rkt")

(provide tref tlen list->tensor number?
         tensor? tensor build-tensor trefs merge-flats)
