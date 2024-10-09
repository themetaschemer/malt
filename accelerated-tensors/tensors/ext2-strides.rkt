#lang racket

(require file/xxhash32)

(provide strides-null
         strides-cons
         (rename-out (ext2-strides-strides strides-strides)
                     (ext2-strides-sign strides-signature)))

(define (strides-signature! ctx strides)
  (xxh32-update!
   ctx
   (for/fold
    ((result #""))
    ((stride-vec strides))
     (match-let* ((`#(,s1 ,s2 ,s3) stride-vec))
       (bytes-append result
                     (integer->integer-bytes s1 4 #f)
                     (integer->integer-bytes s2 4 #f)
                     (integer->integer-bytes s3 4 #f))))))

(struct ext2-strides ((strides #:mutable) sign))

(define strides-null
  (ext2-strides '() (let ((ctx (make-xxh32)))
                      (~r (xxh32-digest ctx) #:base 16))))

(define strides-cons
  (λ (st-out st0 st1 strides)
    (let ((new-list (cons (vector st-out st0 st1)
                          (ext2-strides-strides strides))))
      (ext2-strides new-list
                    (begin
                      (let ((ctx (make-xxh32)))
                        (strides-signature! ctx new-list)
                        (~r (xxh32-digest ctx) #:base 16)))))))
