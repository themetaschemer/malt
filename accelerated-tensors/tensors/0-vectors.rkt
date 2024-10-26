#lang racket
(require ffi/vector)
(require ffi/unsafe)

;;------------------------------------------------
;; Raw representation of vectors
;;------------------------------------------------

(define vec? f32vector?)
(define vec f32vector)
(define make-vec make-f32vector)
(define vref f32vector-ref)
(define vset! f32vector-set!)
(define vlen f32vector-length)
(define list->vec list->f32vector)
(define build-vec
  (λ (n proc)
    (list->vec (map (compose exact->inexact proc) (range n)))))
(define vec->cpointer f32vector->cpointer)
(define vref-cpointer
  (λ (v i)
    (unless (and (<= 0 i) (< i (vlen v)))
      (error 'vref-cpointer
             "Index ~a out of range [0, ~a]"
             i (sub1 (vlen v))))
    (ptr-add (vec->cpointer v) i _float)))

(define-for-syntax debug-leaks? #f)
(define-syntax when-debug-leaks
  (λ (x)
    (syntax-case x ()
      ((when-debug-leaks expr)
       debug-leaks?
       #'expr)
      ((when-debug-leaks expr)
       #'(void)))))

(define new-vec
  (λ (size initial-value [context 'new-vec])
    (let ((m (make-vec size initial-value)))
      (when-debug-leaks (manage-flat-vector! m context))
      m)))

(define vcopy
  (λ (dest idest src isrc n)
    (for ([id (in-range idest (+ n idest))]
          [is (in-range isrc (+ n isrc))])
      (vset! dest id (vref src is)))))

(define print-vec
  (λ (v (off 0) (port (current-output-port)))
    (fprintf port "#(")
    (for ((i (in-range off (vlen v))))
      (fprintf port "~a " (vref v i)))
    (fprintf port ")~n")))

(provide vec? vec vref vset! vlen vcopy print-vec
         list->vec build-vec vec->cpointer vref-cpointer new-vec)

;;------------------------------------------------
;; Memory management for flat-vectors
;;------------------------------------------------

(define flat-vector-manager
  (make-will-executor))

(define manage-flat-vector!
  (λ (m context)
    (set-count! context (add1 (count context)))
    (will-register flat-vector-manager m (flat-vector-collector context))))

(define flat-vector-collector
  (λ (context)
    (λ (v)
      (cond
        ((vector? v)
         (set-count! context (sub1 (count context))))
        (else (fprintf (current-error-port) "?? ..."))))))

(define start-vector-manager
  (λ ()
    (when-debug-leaks
      (void
        (thread
          (λ ()
            (let loop ()
              (will-execute flat-vector-manager)
              (loop))))))))

(define counts (make-hash))
(define count (λ (context) (dict-ref counts context 0)))
(define set-count! (λ (context v) (dict-set! counts context v)))
(define vector-manager-report
  (λ ()
    (fprintf (current-error-port) "----------------------------------------------~%")
    (fprintf (current-error-port) "context\t\t\tcount~%")
    (for ([(context count) (in-hash counts)])
      (fprintf (current-error-port) "~a\t\t\t~a~%" context count))
    (fprintf (current-error-port) "----------------------------------------------~%")))

(provide start-vector-manager vector-manager-report)
