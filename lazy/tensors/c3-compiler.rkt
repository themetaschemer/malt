#lang racket

(require "c0-ast.rkt")
(require (only-in "c2-interpreter.rkt" interp-tensor interp-racket))
(require "../../flat-tensors/ext-impl.rkt")
(require (prefix-in flat: "../../flat-tensors/tensors.rkt"))
(require (only-in "c1-racket-runtime.rkt"
                  runtime ext2-∇-result-res
                  set-ext2-∇-result-res!))
(require rackunit)

(struct counter-data (binding-name ref-count)
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The data segment is a vector that contains
;;  * scalars (arguments to tref)
;;  * flat tensors
;;  * flat tensor¹ of indices that will be the arguments to trefs
;;  * the symbol 'uncalculated as an initial placeholder for the output of
;;    tcomp-ext2-∇ which will be later replaced by the flat tensor output

(define generate-ds-refs
  (λ (t)
    (let-values (((t^ ref) (gdr-tpromise t 0 (make-hasheq))))
      t^)))

(define gdr-tpromise
  (λ (tp ref memo)
    (match tp
      ((tpromise tc s dss sign)
       (let-values (((tc^ ref^) (gdr-tcomp tc ref memo)))
         (values (tpromise tc^ s dss sign) ref^))))))

(define gdr-tcomp
  (λ (tc ref memo)
    (cond
      ((hash-ref memo tc #f)
       =>
       (λ (res/ref-count)
         (match-let (((cons res ref-count) res/ref-count))
           (values res (+ ref ref-count)))))
      (else
       (let-values
           (((res ref^)
             (match tc
               ((? number?) (values tc ref))
               [(tcomp-list->tensor lst)
                (for/fold
                 ((tcs '())
                  (ref^ ref)
                  #:result (values (tcomp-list->tensor (reverse tcs)) ref^))
                 ((l lst))
                  (let-values (((tc ref^^)
                                (cond
                                  ((tpromise? l) (gdr-tpromise l ref^ memo))
                                  ((number? l) (values l ref^))
                                  (else (error 'gdr-list->tensor
                                               "Unexpected: ~a" l)))))
                    (values (cons tc tcs) ref^^)))]
               [(tcomp-tref tp (tcomp-ds-ref #f))
                (let-values (((tp^ ref^) (gdr-tpromise tp ref memo)))
                  (values (tcomp-tref tp^ (tcomp-ds-ref ref^)) (add1 ref^)))]
               [(tcomp-trefs tp (tcomp-ds-ref #f))
                (let-values (((tp^ ref^) (gdr-tpromise tp ref memo)))
                  (values (tcomp-trefs tp^ (tcomp-ds-ref ref^)) (add1 ref^)))]
               [(tcomp-ext2-∇ fᵈ sign r0 r1 shape-fn tp-t0 tp-t1 tp-z
                              out-ref0
                              out-ref1 i)
                (let*-values (((tp-t0^ ref^) (gdr-tpromise tp-t0 ref memo))
                              ((tp-t1^ ref^^) (gdr-tpromise tp-t1 ref^ memo))
                              ((tp-z^ ref^^^) (gdr-tpromise tp-z ref^^ memo)))
                  (cond
                    ((and (eqv? i 0)
                          (not (tcomp-ds-ref-index (ext2-∇-result-res out-ref0))))
                     (set-ext2-∇-result-res! out-ref0 (tcomp-ds-ref ref^^^)))
                    ((and (eqv? i 1)
                          (not (tcomp-ds-ref-index (ext2-∇-result-res out-ref1))))
                     (set-ext2-∇-result-res! out-ref1 (tcomp-ds-ref ref^^^))))
                  (values (tcomp-ext2-∇ fᵈ sign r0 r1 shape-fn tp-t0^ tp-t1^ tp-z^
                                        out-ref0 out-ref1 i)
                          (add1 ref^^^)))]
               [(tcomp-ext1-∇ tp zp f sign m shape-fn)
                (let*-values (((tp^ ref^) (gdr-tpromise tp ref memo))
                              ((zp^ ref^^) (gdr-tpromise zp ref^ memo)))
                  (values (tcomp-ext1-∇ tp^ zp^ f sign m shape-fn) ref^^))]
               [(tcomp-ext2-ρ-scalar f sign tp-t tp-u)
                (let*-values (((tp-t^ ref^) (gdr-tpromise tp-t ref memo))
                              ((tp-u^ ref^^) (gdr-tpromise tp-u ref^ memo)))
                  (values (tcomp-ext2-ρ-scalar f sign tp-t^ tp-u^) ref^^))]
               [(tcomp-ext2-ρ tp-t tp-u f sign m n shape-fn)
                (let*-values (((tp-t^ ref^) (gdr-tpromise tp-t ref memo))
                              ((tp-u^ ref^^) (gdr-tpromise tp-u ref^ memo)))
                  (values (tcomp-ext2-ρ tp-t^ tp-u^ f sign m n shape-fn) ref^^))]
               [(tcomp-ext1-ρ-scalar f sign tp)
                (let-values (((tp^ ref^) (gdr-tpromise tp ref memo)))
                  (values (tcomp-ext1-ρ-scalar f sign tp^) ref^))]
               [(tcomp-ext1-ρ f sign m shape-fn tp)
                (let-values (((tp^ ref^) (gdr-tpromise tp ref memo)))
                  (values (tcomp-ext1-ρ f sign m shape-fn tp^) ref^))]
               [(tcomp-reshape s tp)
                (let-values (((tp^ ref^) (gdr-tpromise tp ref memo)))
                  (values (tcomp-reshape s tp^) ref^))]
               [(tcomp-ds-ref #f) (values (tcomp-ds-ref ref) (add1 ref))]
               ;;need these cases for testing compiler invariant
               [(tcomp-let lhs rhs body)
                (let*-values (((rhs^ ref^) (gdr-tpromise rhs ref memo))
                              ((body^ ref^^) (gdr-tpromise body ref^ memo)))
                  (values (tcomp-let lhs rhs^ body^) ref^^))]
               [(tcomp-var name) (values (tcomp-var name) ref)])))
         (hash-set! memo tc (cons res (- ref^ ref)))
         (values res ref^))))))

;; Count references so that the tcomp AST nodes that refer to the same memory
;; location i.e. common AST nodes get extracted by let-binding them in the
;; compiled output racket code.
(define count-references
  (λ (t)
    (let-values (((counter uid) (cr-tpromise t (hasheq) 0)))
      counter)))

;; TODO: Try using the signature field of tpromise struct as keys instead tcomp
;; references
(define cr-tpromise
  (λ (t counter uid)
    (match t
      ((tpromise tc _ _ _)
       (cr-tcomp tc counter uid)))))

(define cr-tcomp
  (λ (tc counter uid)
    (cond
      ((number? tc) (values counter uid))
      (else
       (match-let (((counter-data tc-binding-name tc-ref-count)
                    (hash-ref counter tc
                              (λ ()
                                (let-values (((st _) (struct-info tc)))
                                  (let-values (((tcomp-name _0 _1 _2 _3 _4 _5 _6)
                                                (struct-type-info st)))
                                    (counter-data (string->symbol
                                                   (format "~a_~a" tcomp-name uid))
                                                  0)))))))
         (let* ((new-count (add1 tc-ref-count))
                (counter^ (hash-set counter tc
                                    (counter-data tc-binding-name
                                                  new-count)))
                (uid^ (add1 uid)))
           (cond
             ((> new-count 1)
              ;; No need to increase reference count of children if parent occurs
              ;; more than once. This helps avoid creating extra tcom-var later in
              ;; ecs if child tcomp occurs only once within the parent tcomp, but
              ;; the parent tcomp itself occurs more than once.
              (values counter^ uid^))
             (else
              (match tc
                [(tcomp-list->tensor lst)
                 (for/fold
                  ((counter^^ counter^)
                   (uid^^ uid^))
                  ((l lst))
                   (cond
                     ((tpromise? l) (cr-tpromise l counter^^ uid^^))
                     ((number? l) (values counter^^ uid^^))
                     (else (error 'cr-list->tensor "Unexpected: ~a" l))))]
                [(tcomp-tref tp i)
                 (cr-tpromise tp counter^ uid^)]
                [(tcomp-trefs tp b)
                 (cr-tpromise tp counter^ uid^)]
                [(tcomp-ext2-∇ fᵈ sign r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
                 (let*-values (((counter-1 uid-1) (cr-tpromise tp-t0 counter^ uid^))
                               ((counter-2 uid-2) (cr-tpromise tp-z counter-1 uid-1)))
                   (cr-tpromise tp-t1 counter-2 uid-2))]
                [(tcomp-ext1-∇ tp zp f sign m shape-fn)
                 (let-values (((counter-1 uid-1) (cr-tpromise tp counter^ uid^)))
                   (cr-tpromise zp counter-1 uid-1))]
                [(tcomp-ext2-ρ-scalar f sign tp-t tp-u)
                 (let-values (((counter-1 uid-1) (cr-tpromise tp-t counter^ uid^)))
                   (cr-tpromise tp-u counter-1 uid-1))]
                [(tcomp-ext2-ρ tp-t tp-u f sign m n shape-fn)
                 (let-values (((counter-1 uid-1) (cr-tpromise tp-t counter^ uid^)))
                   (cr-tpromise tp-u counter-1 uid-1))]
                [(tcomp-ext1-ρ-scalar f sign tp)
                 (cr-tpromise tp counter^ uid^)]
                [(tcomp-ext1-ρ f sign m shape-fn tp)
                 (cr-tpromise tp counter^ uid^)]
                [(tcomp-reshape s tp)
                 (cr-tpromise tp counter^ uid^)]
                [(tcomp-ds-ref index) (values counter^ uid^)]
                ;;need these cases for testing compiler invariant
                [(tcomp-let lhs rhs body)
                 (let-values (((counter-1 uid-1) (cr-tpromise rhs counter^ uid^)))
                   (cr-tpromise body counter-1 uid-1))]
                [(tcomp-var name) (values counter^ uid^)])))))))))

(define extract-common-subexpressions
  (λ (t counter)
    (let-values (((instrs bindings)
                  (run-compiler-ecs (ecs-tpromise t counter) '())))
      (for/fold ((body instrs))
                ((binding bindings))
        (tpromise (tcomp-let (car binding)
                             (tpromise (cdr binding) '() (box '()) (box '()))
                             body)
                  '() (box '()) (box '()))))))

(define ecs-tpromise
  (λ (tc counter)
    (match tc
      [(tpromise tc s dss sign)
       (->ecs
        (ecs-tcomp tc counter)
        (λ (instrs)
          (inj-ecs-val (tpromise instrs s dss sign))))])))

(define ecs-tcomp
  (λ (tc counter)
    (let ((tc-counter-data
           (hash-ref counter tc
                     (λ ()
                       (counter-data (gensym 'illegal) 0)))))
      (match tc
        [tc #:when (number? tc)
            (inj-ecs-val tc)]
        [(tcomp-list->tensor lst)
         (let ((instrs-list-compiler
                (for/foldr
                  ((list-compiler (inj-ecs-val '())))
                  ((arg lst))
                  (->ecs
                   (cond
                     ((tpromise? arg) (ecs-tpromise arg counter))
                     ((number? arg) (inj-ecs-val arg))
                     (else (error 'ecs-list->tensor "Unexpected: ~a" arg)))
                   (λ (instrs)
                     (->ecs
                      list-compiler
                      (λ (instrs-list)
                        (inj-ecs-val (cons instrs instrs-list)))))))))
           (->ecs
            instrs-list-compiler
            (λ (instrs-list)
              (inj-ecs-tcomp (tcomp-list->tensor instrs-list) tc-counter-data))))]
        [(tcomp-tref tp i)
         (->ecs
          (ecs-tpromise tp counter)
          (λ (instrs)
            (inj-ecs-tcomp (tcomp-tref instrs i) tc-counter-data)))]
        [(tcomp-trefs tp b)
         (->ecs
          (ecs-tpromise tp counter)
          (λ (instrs)
            (inj-ecs-tcomp (tcomp-trefs instrs b) tc-counter-data)))]
        [(tcomp-ext2-∇ fᵈ sign r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
         (->ecs
          (ecs-tpromise tp-t0 counter)
          (λ (t0-instrs)
            (->ecs
             (ecs-tpromise tp-t1 counter)
             (λ (t1-instrs)
               (->ecs
                (ecs-tpromise tp-z counter)
                (λ (z-instrs)
                  (inj-ecs-tcomp
                   (tcomp-ext2-∇ fᵈ sign r0 r1 shape-fn
                                 t0-instrs t1-instrs z-instrs
                                 out0 out1 i)
                   tc-counter-data)))))))]
        [(tcomp-ext1-∇ tp zp f sign m shape-fn)
         (->ecs
          (ecs-tpromise tp counter)
          (λ (t-instrs)
            (->ecs
             (ecs-tpromise zp counter)
             (λ (z-instrs)
               (inj-ecs-tcomp
                (tcomp-ext1-∇ t-instrs z-instrs f sign m shape-fn)
                tc-counter-data)))))]
        [(tcomp-ext2-ρ-scalar f sign tp-t tp-u)
         (->ecs
          (ecs-tpromise tp-t counter)
          (λ (t-instrs)
            (->ecs
             (ecs-tpromise tp-u counter)
             (λ (u-instrs)
               (inj-ecs-tcomp
                (tcomp-ext2-ρ-scalar f sign t-instrs u-instrs)
                tc-counter-data)))))]
        [(tcomp-ext2-ρ tp-t tp-u f sign m n shape-fn)
         (->ecs
          (ecs-tpromise tp-t counter)
          (λ (t-instrs)
            (->ecs
             (ecs-tpromise tp-u counter)
             (λ (u-instrs)
               (inj-ecs-tcomp
                (tcomp-ext2-ρ t-instrs u-instrs f sign m n shape-fn)
                tc-counter-data)))))]
        [(tcomp-ext1-ρ-scalar f sign tp)
         (->ecs
          (ecs-tpromise tp counter)
          (λ (instrs)
            (inj-ecs-tcomp (tcomp-ext1-ρ-scalar f sign instrs) tc-counter-data)))]
        [(tcomp-ext1-ρ f sign m shape-fn tp)
         (->ecs
          (ecs-tpromise tp counter)
          (λ (instrs)
            (inj-ecs-tcomp (tcomp-ext1-ρ f sign m shape-fn instrs) tc-counter-data)))]
        [(tcomp-reshape s tp)
         (->ecs
          (ecs-tpromise tp counter)
          (λ (instrs)
            (inj-ecs-tcomp (tcomp-reshape s instrs) tc-counter-data)))]
        [(tcomp-ds-ref index) (inj-ecs-tcomp tc tc-counter-data)]))))

(struct CompilerECS (run-compiler) #:transparent)

(define run-compiler-ecs
  (λ (c bindings)
    ((CompilerECS-run-compiler c) bindings)))

(define inj-ecs-val
  (λ (v)
    (CompilerECS (λ (bindings) (values v bindings)))))

(define inj-ecs-tcomp
  (λ (instrs cd)
    (match-let (((counter-data binding-var ref-count) cd))
      (CompilerECS (λ (bindings)
                  (cond
                    ((<= ref-count 1)
                     (values instrs bindings))
                    ((assv binding-var bindings)
                     (values (tcomp-var binding-var) bindings))
                    (else
                     (values (tcomp-var binding-var)
                             (extend-bindings binding-var instrs bindings)))))))))

(define ->ecs
  (λ (c f)
    (CompilerECS
     (λ (bindings)
       (let-values (((instrs bindings^) (run-compiler-ecs c bindings)))
         (run-compiler-ecs (f instrs) bindings^))))))

(define extend-env
  (λ (k v env)
    `((,k . ,v) . ,env)))

(define extend-bindings extend-env)

(define exists-in-env?
  (λ (ft env)
    (match env
      ('() #f)
      (`((,k . ,v) . ,_) #:when (eq? ft v) k)
      (`(,_ . ,rest-env) (exists-in-env? ft rest-env)))))

(define generate-racket
  (λ (t)
    (gr-tpromise t)))

(define gr-tpromise
  (λ (t)
    (match t
      [(tpromise tc _ _ _) (gr-tcomp tc)])))

(define gr-tcomp
  (λ (tc)
    (match tc
      [v #:when (number? v) v]
      [(tcomp-list->tensor lst)
       (let ((instrs-list (map (λ (t)
                                 (cond
                                   ((tpromise? t) (gr-tpromise t))
                                   ((number? t) t)
                                   (else (error 'gr-list->tensor "Unexpected: ~a" t))))
                               lst)))
         `(flat:list->tensor (list ,@instrs-list)))]
      [(tcomp-tref tp i)
       (let ((instrs (gr-tpromise tp))
             (i-instrs (gr-tcomp i)))
         `(flat:tref ,instrs ,i-instrs))]
      [(tcomp-trefs tp b)
       (let ((instrs (gr-tpromise tp))
             (b-instrs (gr-tcomp b)))
         `(rt:trefs ,instrs ,b-instrs))]
      [(tcomp-ext2-∇ fᵈ sign r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
       (let ((t0-instrs (gr-tpromise tp-t0))
             (t1-instrs (gr-tpromise tp-t1))
             (z-instrs (gr-tpromise tp-z))
             (out-idx0 (tcomp-ds-ref-index (ext2-∇-result-res out0)))
             (out-idx1 (tcomp-ds-ref-index (ext2-∇-result-res out1))))
         (let ((index (if (zero? i) out-idx0 out-idx1)))
           `(let* ([index ,index]
                   [v (data-segment-ref index)])
              (cond
                ((eqv? v 'uncalculated)
                 (ext2-∇-forcer ,fᵈ ,r0 ,r1 ,shape-fn
                                ,t0-instrs ,t1-instrs
                                ,z-instrs ,out-idx0 ,out-idx1)
                 (data-segment-ref index))
                (else v)))))]
      [(tcomp-ext1-∇ tp zp f sign m shape-fn)
       (let ((t-instrs (gr-tpromise tp))
             (z-instrs (gr-tpromise zp)))
         `(scalarize
           (flat-ext1-∇ ,f ,m ,shape-fn
                        (ensure-flat ,t-instrs)
                        (ensure-flat ,z-instrs))))]
      [(tcomp-ext2-ρ-scalar f sign tp-t tp-u)
       (let ((t-instrs (gr-tpromise tp-t))
             (u-instrs (gr-tpromise tp-u)))
         `(,f ,t-instrs ,u-instrs))]
      [(tcomp-ext2-ρ tp-t tp-u f sign m n shape-fn)
       (let ((t-instrs (gr-tpromise tp-t))
             (u-instrs (gr-tpromise tp-u)))
         `(scalarize
           (flat-ext2-ρ ,f ,m ,n ,shape-fn
                        (ensure-flat ,t-instrs)
                        (ensure-flat ,u-instrs))))]
      [(tcomp-ext1-ρ-scalar f sign tp)
       (let ((instrs (gr-tpromise tp)))
         `(,f ,instrs))]
      [(tcomp-ext1-ρ f sign m shape-fn tp)
       (let ((instrs (gr-tpromise tp)))
         `(scalarize
           (flat-ext1-ρ ,f ,m ,shape-fn
                        (ensure-flat ,instrs))))]
      [(tcomp-reshape s tp)
       (let ((instrs (gr-tpromise tp)))
         `(flat ',s
                (flat-store ,instrs)
                (flat-offset ,instrs)))]
      [(tcomp-let lhs rhs body)
       (let ((rhs-instrs (gr-tpromise rhs))
             (body-instrs (gr-tpromise body)))
         `(let ((,lhs ,rhs-instrs))
            ,body-instrs))]
      [(tcomp-var name) name]
      [(tcomp-ds-ref index) `(data-segment-ref ,index)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composing Compiler Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define print-compiler? (make-parameter #f))
(define display-compiler-trace
  (λ (title value)
    (when (or (equal? (print-compiler?) #t)
              (and (list? (print-compiler?)) (member title (print-compiler?))))
      (printf "~a:~n" title)
      (displayln "--------------")
      (pretty-print value)
      (displayln ""))))
(define cache
  (make-parameter (make-hash)))
(define compile-tensor
  (λ (t)
    (display-compiler-trace 'Source-Tensor t)
    (let ((ds (dst->data-segment (tpromise-dst t))))
      (display-compiler-trace 'Data-Segment ds)
      (let ((signature (sign (tpromise-sign t))))
        (display-compiler-trace 'Signature signature)
        (cond
          ((hash-has-key? (cache) signature)
           (let ((compiled (hash-ref (cache) signature)))
             (display-compiler-trace 'Cache-Hit signature)
             (values compiled ds)))
          (else
           (let ((instrs-dsr (generate-ds-refs t)))
               (display-compiler-trace 'Generate-DS-Refs instrs-dsr)
             (let ((counter (count-references instrs-dsr)))
               (display-compiler-trace 'Count-References counter)
               (let ((extracted (extract-common-subexpressions instrs-dsr counter)))
                 (display-compiler-trace 'Extract-Common-Subexpressions extracted)
                 (let* ((gr (generate-racket extracted))
                        (rkt (compile-racket gr)))
                   (display-compiler-trace 'Generate-Racket gr)
                   (hash-set! (cache) signature rkt)
                   (values rkt ds)))))))))))

(define compile-racket
  (λ (r)
    (parameterize ([current-namespace runtime])
      (compile-syntax (expand r)))))

;;TODO: update this for new compiler passes
(define compile-tensor/checks
  (λ (t)
    t
    #;
    (let-values (((eds-instrs ds) (extract-data-segment t)))
      (flat:check-tensor-equal? (interp-tensor t) (interp-tensor eds-instrs))
      (let ((counter (count-references t)))
        (let ((extracted (extract-common-subexpressions t counter)))
          (flat:check-tensor-equal? (interp-tensor t) (interp-tensor extracted))
          (for/list ((cd (hash-values (count-references extracted))))
            (check-equal? (counter-data-ref-count cd) 1))
          (let-values (((rkt env) (generate-racket extracted)))
            (flat:check-tensor-equal? (interp-tensor extracted)
                                      (interp-racket rkt env))
            (values rkt env)))))))

(define get-compiled
  (λ (t)
    (let-values (((instrs env)
                  (compile-tensor t)))
      `(parameterize ((data-segment ,env))
         ,instrs))))

(include "test/test-c3-compiler.rkt")
(provide get-compiled compile-tensor compile-tensor/checks print-compiler?
         (rename-out (cache compiler-cache)))
