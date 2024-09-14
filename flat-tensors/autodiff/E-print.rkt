#lang racket

(require "A-autodiff.rkt")
(require "../tensors.rkt")

(define max-tensor-print-length (make-parameter 5))

(struct fake-tensor (members)
  #:transparent
  #:methods gen:custom-write
  ((define write-proc
     (λ (fake-tensor port mode)
       (let ((n (length (fake-tensor-members fake-tensor))))
         (case mode
           ((#t)
            (display "(tensor " port)
            (for ([m (fake-tensor-members fake-tensor)]
                  [c (in-range 0 n)])
              (if (symbol? m)
                  (display m port)
                  (write m port))
              (when (< c (- n 1))
                (display " " port)))
            (display ")" port))
           ((#f)
            (display "(tensor " port)
            (for ([m (fake-tensor-members fake-tensor)]
                  [c (in-range 0 n)])
              (display m port)
              (when (< c (- n 1))
                (display " " port)))
            (display ")" port))
           (else
            (display "(tensor " port)
            (for ([m (fake-tensor-members fake-tensor)]
                  [c (in-range 0 n)])
              (if (symbol? m)
                  (display m port)
                  (print m port mode))
              (when (< c (- n 1))
                (display " " port)))
            (display ")" port))))))))

(define make-printable
  (λ (y [max-length (max-tensor-print-length)])
    (cond
      ((dual? y) (make-printable (ρ y)))
      ((flat? y) (make-printable-flat y max-length))
      ((list? y)
       (map (λ (le) (make-printable le max-length)) y))
      ((vector? y)
       (vector-map (λ (ve) (make-printable ve max-length)) y))
      (else y))))

(define make-printable-flat
  (λ (y max-length)
    (flat->tensor-list
      (flat-store y) (flat-offset y) (flat-shape y)
      (strides (flat-shape y)) max-length)))

(define flat->tensor-list
  (λ (store offset shape strides max-length)
    (cond
      ((null? shape) (vector-ref store offset))
      (else
       (let ((top-len (car shape))
             (stride (car strides)))
         (fake-tensor
           (reverse
             (call/cc
               (λ (return)
                 (for/fold ((lst '())) ((i (in-range offset (+ offset (* top-len stride)) stride))
                                        (count (in-naturals 0)))
                   (cond
                     ((and (> max-length 0) (= count max-length)) (return (cons '... lst)))
                     (else
                      (cons (flat->tensor-list store i (cdr shape) (cdr strides) max-length)
                            lst)))))))))))))

(include "test/test-E-print.rkt")

(provide max-tensor-print-length
         make-printable
         ;; This is used in ext-impl.rkt
         make-printable-flat
         fake-tensor)
