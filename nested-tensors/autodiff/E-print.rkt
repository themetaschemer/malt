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
      ((list? y)
       (map (λ (le) (make-printable le max-length)) y))
      ((and (vector? y)
            (call/cc (λ (finally)
                       (tensor-like-vector? y finally))))
       (vector->tensor-list y max-length))
      ((vector? y)
       (vector-map (λ (ve) (make-printable ve max-length)) y))
      (else y))))

(define vector->tensor-list
  (λ (y max-length)
    (fake-tensor
      (reverse
        (call/cc
          (λ (return)
            (for/fold ([lst '()][len 0] #:result lst) ([ve y])
              (cond
                ((and (> max-length 0) (= len max-length)) (return (cons '... lst)))
                (else (values
                       (cons (make-printable ve max-length) lst)
                       (add1 len)))))))))))

(define tensor-like-vector?
  (λ (v finally)
    (cond
      ((number? v) (list))
      ((vector? v)
       (for/fold ((len 0)
                  (tensor-shape #f) #:result (cons len tensor-shape)) ((ve v))
         (cond
           (tensor-shape
            (if (equal? tensor-shape (tensor-like-vector? ve finally))
                (values (add1 len) tensor-shape)
                (finally #f)))
           (else (values (add1 len) (tensor-like-vector? ve finally))))))
      (else (finally #f)))))

(include "test/test-E-print.rkt")

(provide max-tensor-print-length
         make-printable)
