#lang racket

(require "c0-ast.rkt")
(require (only-in "c2-interpreter.rkt" make-instrs interp-tensor interp-racket))
(require "../../flat-tensors/ext-impl.rkt")
(require (prefix-in flat: "../../flat-tensors/tensors.rkt"))
(require rackunit)
(require file/xxhash32)

(struct counter-data (binding-name
                      ref-count)
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO: later eds and gs passes should not be needed because the tcomp AST nodes
;;should have a signature and dss field which will be populated at the time of
;;their instantiation. Then we just access those fields from the AST node rather
;;than computing them. Use a global data segment that has flat tensors used by all tcomp nodes in our program.

;;Extracts the data segment which is a vector that contains scalars (arguments
;;to tref), flat tensors and flat tensor¹ of indices that will be the arguments
;;to trefs.
(define extract-data-segment
  (λ (t)
    (let-values (((t^ data-segment-stack) (eds-expr t '())))
      ;; convert data segment stack to data segment array
      (values t^ (list->vector (reverse data-segment-stack))))))

(define eds-expr
  (λ (t dss)
    (match t
      (s #:when (number? s)
        (values s dss))
      (ft
       #:when (flat? ft)
       (cond
         ((memq ft dss)
          =>
          (λ (res)
            (values (tcomp-ds-ref (length (cdr res))) dss)))
         (else (values (tcomp-ds-ref (length dss)) (cons ft dss)))))
      ((tpromise tc s)
       (let-values (((tc^ dss^) (eds-expr tc dss)))
         (cond
           ((number? tc^) (values tc^ dss^))
           (else (values (tpromise tc^ s) dss^)))))
      ((tcomp) (eds-tcomp t dss)))))

(define eds-tcomp
  (λ (tc dss)
    (match tc
      [(tcomp-list->tensor lst)
       (let-values (((ts dss^)
                     (for/fold ((ts '())
                                (dss^ dss))
                               ((l lst))
                       (let-values (((t dss^^) (eds-expr l dss^)))
                         (values (cons t ts) dss^^)))))
         (values (tcomp-list->tensor (reverse ts)) dss^))]
      [(tcomp-build-tensor s f)
       (values tc dss)]
      [(tcomp-tref tp i)
       (let-values (((t dss^) (eds-expr tp dss)))
         (values (tcomp-tref t (tcomp-ds-ref (length dss^)))
                 (cons i dss^)))]
      [(tcomp-trefs tp b)
       (let-values (((t dss^) (eds-expr tp dss)))
         (values (tcomp-trefs t (tcomp-ds-ref (length dss^)))
                 (cons (flat:list->tensor b) dss^)))]
      [(tcomp-ext2-∇ fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
       (let-values (((t0 dss^) (eds-expr tp-t0 dss)))
         (let-values (((t1 dss^^) (eds-expr tp-t1 dss^)))
           (let-values (((z dss^^^) (eds-expr tp-z dss^^)))
             (values (tcomp-ext2-∇ fᵈ r0 r1 shape-fn t0 t1 z out0 out1 i)
                     dss^^^))))]
      [(tcomp-ext1-∇ tp zp f m shape-fn)
       (let-values (((tp^ dss^) (eds-expr tp dss)))
         (let-values (((zp^ dss^^) (eds-expr zp dss^)))
           (values (tcomp-ext1-∇ tp^ zp^ f m shape-fn) dss^^)))]
      [(tcomp-ext2-ρ-scalar f tp-t tp-u)
       (let-values (((t dss^) (eds-expr tp-t dss)))
         (let-values (((u dss^^) (eds-expr tp-u dss^)))
           (values (tcomp-ext2-ρ-scalar f t u) dss^^)))]
      [(tcomp-ext2-ρ tp-t tp-u f m n shape-fn)
       (let-values (((t dss^) (eds-expr tp-t dss)))
         (let-values (((u dss^^) (eds-expr tp-u dss^)))
           (values (tcomp-ext2-ρ t u f m n shape-fn) dss^^)))]
      [(tcomp-ext1-ρ-scalar f tp)
       (let-values (((tp^ dss^) (eds-expr tp dss)))
           (values (tcomp-ext1-ρ-scalar f tp^) dss^))]
      [(tcomp-ext1-ρ f m shape-fn tp)
       (let-values (((tp^ dss^) (eds-expr tp dss)))
           (values (tcomp-ext1-ρ f m shape-fn tp^) dss^))]
      [(tcomp-reshape s tp)
       (let-values (((tp^ dss^) (eds-expr tp dss)))
           (values (tcomp-reshape s tp^) dss^))])))

(define hash-signatures?
  (make-parameter #t))
;;TODO: Optimize sign by replacing it with this commented function
#;
(define sign
  (let ((xxh32-ctx (make-xxh32)))
    (λ ss
      (cond
        ((hash-signatures?)
         (xxh32-reset! xxh32-ctx 0)
         (xxh32-update! xxh32-ctx (apply bytes-append ss))
         (xxh32-digest xxh32-ctx))
        (else (format "~a" ss))))))
(define sign
  (let ((xxh32-ctx (make-xxh32)))
    (λ (s)
      (cond
        ((hash-signatures?)
         (xxh32-reset! xxh32-ctx 0)
         (xxh32-update! xxh32-ctx (string->bytes/utf-8 s))
         (format "~a" (xxh32-digest xxh32-ctx)))
        (else (format "~a" s))))))

#;
(define generate-signature
  (λ (t)
    (gs-expr t '())))
(define generate-signature
  (λ (t)
    (gs-expr t)))

#;
(define gs-expr
  (λ (t position)
    (match t
      (s #:when (number? s)
        (sign (format "s~a~a" s position)))
      ((tpromise tc _) (gs-expr tc (cons 0 position)))
      ((tcomp) (gs-tcomp t position)))))
(define gs-expr
  (λ (t)
    (match t
      (s #:when (number? s)
        (sign (format "s~a" s)))
      ((tpromise tc _) (gs-expr tc))
      ((tcomp) (gs-tcomp t)))))

#;
(define gs-tcomp
  (λ (tc position)
    (match tc
      [(tcomp-list->tensor lst)
       (let ((list-sig
              (for/fold ((sig ""))
                        ((l lst)
                         (i (in-naturals 0)))
                (string-append sig (gs-expr l (cons i position))))))
         (sign (format "l>t~a~a" list-sig position)))]
      [(tcomp-build-tensor s f)
       (sign (format "bt~a~a~a" s f position))]
      [(tcomp-tref tp i)
       (sign
        (format "tr~a~a~a"
                (gs-expr tp (cons 0 position))
                (gs-expr i (cons 1 position))
                position))]
      [(tcomp-trefs tp b)
       (sign
        (format "trs~a~a~a"
         (gs-expr tp (cons 0 position))
         (gs-expr b (cons 1 position))
         position))]
      [(tcomp-ext2-∇ fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
       (sign
        (format "e2∇~a~a_~a~a~a~a~a~a~a"
                fᵈ r0 r1 shape-fn
                (gs-expr tp-t0 (cons 0 position))
                (gs-expr tp-t1 (cons 1 position))
                (gs-expr tp-z (cons 2 position))
                i
                position))]
      [(tcomp-ext1-∇ tp zp f m shape-fn)
       (sign
        (format "e1∇~a~a~a~a~a~a"
                f m shape-fn
                (gs-expr tp (cons 0 position))
                (gs-expr zp (cons 1 position))
                position))]
      [(tcomp-ext2-ρ-scalar f tp-t tp-u)
       (sign
        (format "e2ρs~a~a~a~a"
                f
                (gs-expr tp-t (cons 0 position))
                (gs-expr tp-u (cons 1 position))
                position))]
      [(tcomp-ext2-ρ tp-t tp-u f m n shape-fn)
       (sign
        (format "e2ρ~a~a_~a~a~a~a~a"
                f m n shape-fn
                (gs-expr tp-t (cons 0 position))
                (gs-expr tp-u (cons 1 position))
                position))]
      [(tcomp-ext1-ρ-scalar f tp)
       (sign
        (format "e1ρs~a~a~a"
                f
                (gs-expr tp (cons 0 position))
                position))]
      [(tcomp-ext1-ρ f m shape-fn tp)
       (sign
        (format "e1ρ~a~a~a~a~a"
                f m shape-fn
                (gs-expr tp (cons 0 position))
                position))]
      [(tcomp-reshape s tp)
       (sign
        (format "r~a~a~a"
                s (gs-expr tp (cons 0 position)) position))]
      [(tcomp-ds-ref index)
       (sign
        (format "dsr~a~a" index position))])))
(define gs-tcomp
  (λ (tc)
    (match tc
      [(tcomp-list->tensor lst)
       (let ((list-sig
              (for/fold ((sig ""))
                        ((l lst)
                         (i (in-naturals 0)))
                (string-append sig (gs-expr l)))))
         (sign (format "l>t~a" list-sig)))]
      [(tcomp-build-tensor s f)
       (sign (format "bt~a~a" s f))]
      [(tcomp-tref tp i)
       (sign
        (format "tr~a~a" (gs-expr tp ) (gs-expr i )))]
      [(tcomp-trefs tp b)
       (sign
        (format "trs~a~a" (gs-expr tp ) (gs-expr b )))]
      [(tcomp-ext2-∇ fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
       (sign
        (format "e2∇~a~a_~a~a~a~a~a~a"
                fᵈ r0 r1 shape-fn
                (gs-expr tp-t0 )
                (gs-expr tp-t1 )
                (gs-expr tp-z )
                i))]
      [(tcomp-ext1-∇ tp zp f m shape-fn)
       (sign
        (format "e1∇~a~a~a~a~a"
                f m shape-fn
                (gs-expr tp )
                (gs-expr zp )))]
      [(tcomp-ext2-ρ-scalar f tp-t tp-u)
       (sign
        (format "e2ρs~a~a~a"
                f
                (gs-expr tp-t )
                (gs-expr tp-u )))]
      [(tcomp-ext2-ρ tp-t tp-u f m n shape-fn)
       (sign
        (format "e2ρ~a~a_~a~a~a~a"
                f m n shape-fn
                (gs-expr tp-t )
                (gs-expr tp-u )))]
      [(tcomp-ext1-ρ-scalar f tp)
       (sign
        (format "e1ρs~a~a" f (gs-expr tp )))]
      [(tcomp-ext1-ρ f m shape-fn tp)
       (sign
        (format "e1ρ~a~a~a~a"
                f m shape-fn
                (gs-expr tp )))]
      [(tcomp-reshape s tp)
       (sign
        (format "r~a~a"
                s (gs-expr tp ) ))]
      [(tcomp-ds-ref index)
       (sign
        (format "dsr~a" index ))])))

;; Count references so that the tcomp AST nodes that refer to the same memory
;; location i.e. common AST nodes get extracted by let-binding them in the
;; compiled output racket code.
(define count-references
  (λ (t)
    (count-references-expr t (hasheq))))

(define count-references-expr
  (λ (t counter)
    (match t
      ((tpromise tc _)
       (count-references-expr tc counter))
      ((tcomp) (count-references-tcomp t counter))
      (_ counter))))

(define count-references-tcomp
  (λ (tc counter)
    (match-let (((counter-data tc-binding-name tc-ref-count)
                 (hash-ref counter tc
                           (λ ()
                             (let-values (((st _) (struct-info tc)))
                               (let-values (((tcomp-name _0 _1 _2 _3 _4 _5 _6)
                                             (struct-type-info st)))
                                 (counter-data (gensym tcomp-name) 0)))))))
      (let* ((new-count (add1 tc-ref-count))
             (counter^ (hash-set counter tc
                                 (counter-data tc-binding-name
                                               new-count))))
        (cond
          ((> new-count 1)
           ;; No need to increase reference count of children if parent occurs
           ;; more than once. This helps avoid creating extra tcom-var later in
           ;; ecs if child tcomp occurs only once within the parent tcomp, but
           ;; the parent tcomp itself occurs more than once.
           counter^)
          (else
           (match tc
             [(tcomp-list->tensor lst)
              (for/fold
               ((counter^^ counter^))
               ((l lst))
                (count-references-expr l counter^^))]
             [(tcomp-build-tensor s f) counter^]
             [(tcomp-tref tp i)
              (count-references-expr tp counter^)]
             [(tcomp-trefs tp b)
              (count-references-expr tp counter^)]
             [(tcomp-ext2-∇ fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
              (count-references-expr
               tp-z
               (count-references-expr
                tp-t1
                (count-references-expr tp-t0 counter^)))]
             [(tcomp-ext1-∇ tp zp f m shape-fn)
              (count-references-expr
               zp
               (count-references-expr tp counter^))]
             [(tcomp-ext2-ρ-scalar f tp-t tp-u)
              (count-references-expr
               tp-u
               (count-references-expr tp-t counter^))]
             [(tcomp-ext2-ρ tp-t tp-u f m n shape-fn)
              (count-references-expr
               tp-u
               (count-references-expr tp-t counter^))]
             [(tcomp-ext1-ρ-scalar f tp)
              (count-references-expr tp counter^)]
             [(tcomp-ext1-ρ f m shape-fn tp)
              (count-references-expr tp counter^)]
             [(tcomp-reshape s tp)
              (count-references-expr tp counter^)]
             [(tcomp-ds-ref index) counter^]
             ;;need these cases for testing compiler invariant
             [(tcomp-let lhs rhs body)
              (count-references-expr
               body
               (count-references-expr rhs counter^))]
             [(tcomp-var name) counter^])))))))

(define extract-common-subexpressions
  (λ (t counter)
    (let-values (((instrs bindings)
                  (run-compiler-ecs (ecs-expr t counter) '())))
      (for/fold ((body instrs))
                ((binding bindings))
        (tcomp-let (car binding) (cdr binding) body)))))

(define ecs-expr
  (λ (tc counter)
    (match tc
      [(tpromise tc s)
       (->ecs
        (ecs-expr tc counter)
        (λ (instrs)
          (inj-ecs-val (tpromise instrs s))))]
      [tc #:when (number? tc)
          (inj-ecs-val tc)]
      [(tcomp) (ecs-tcomp tc counter)])))

(define ecs-tcomp
  (λ (tc counter)
    (let ((tc-counter-data
           (hash-ref counter tc
                     (λ ()
                       (counter-data (gensym 'illegal) 0)))))
      (match tc
        [(tcomp-list->tensor lst)
         (let ((instrs-list-compiler
                (for/foldr
                  ((list-compiler (inj-ecs-val '())))
                  ((arg lst))
                  (->ecs
                   (ecs-expr arg counter)
                   (λ (instrs)
                     (->ecs
                      list-compiler
                      (λ (instrs-list)
                        (inj-ecs-val (cons instrs instrs-list)))))))))
           (->ecs
            instrs-list-compiler
            (λ (instrs-list)
              (inj-ecs-tcomp (tcomp-list->tensor instrs-list) tc-counter-data))))]
        [(tcomp-build-tensor s f)
         (inj-ecs-tcomp tc tc-counter-data)]
        [(tcomp-tref tp i)
         (->ecs
          (ecs-expr tp counter)
          (λ (instrs)
            (inj-ecs-tcomp (tcomp-tref instrs i) tc-counter-data)))]
        [(tcomp-trefs tp b)
         (->ecs
          (ecs-expr tp counter)
          (λ (instrs)
            (inj-ecs-tcomp (tcomp-trefs instrs b) tc-counter-data)))]
        [(tcomp-ext2-∇ fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
         (->ecs
          (ecs-expr tp-t0 counter)
          (λ (t0-instrs)
            (->ecs
             (ecs-expr tp-t1 counter)
             (λ (t1-instrs)
               (->ecs
                (ecs-expr tp-z counter)
                (λ (z-instrs)
                  (inj-ecs-tcomp
                   (tcomp-ext2-∇ fᵈ r0 r1 shape-fn
                                 t0-instrs t1-instrs z-instrs
                                 out0 out1 i)
                   tc-counter-data)))))))]
        [(tcomp-ext1-∇ tp zp f m shape-fn)
         (->ecs
          (ecs-expr tp counter)
          (λ (t-instrs)
            (->ecs
             (ecs-expr zp counter)
             (λ (z-instrs)
               (inj-ecs-tcomp
                (tcomp-ext1-∇ t-instrs z-instrs f m shape-fn)
                tc-counter-data)))))]
        [(tcomp-ext2-ρ-scalar f tp-t tp-u)
         (->ecs
          (ecs-expr tp-t counter)
          (λ (t-instrs)
            (->ecs
             (ecs-expr tp-u counter)
             (λ (u-instrs)
               (inj-ecs-tcomp
                (tcomp-ext2-ρ-scalar f t-instrs u-instrs)
                tc-counter-data)))))]
        [(tcomp-ext2-ρ tp-t tp-u f m n shape-fn)
         (->ecs
          (ecs-expr tp-t counter)
          (λ (t-instrs)
            (->ecs
             (ecs-expr tp-u counter)
             (λ (u-instrs)
               (inj-ecs-tcomp
                (tcomp-ext2-ρ t-instrs u-instrs f m n shape-fn)
                tc-counter-data)))))]
        [(tcomp-ext1-ρ-scalar f tp)
         (->ecs
          (ecs-expr tp counter)
          (λ (instrs)
            (inj-ecs-tcomp (tcomp-ext1-ρ-scalar f instrs) tc-counter-data)))]
        [(tcomp-ext1-ρ f m shape-fn tp)
         (->ecs
          (ecs-expr tp counter)
          (λ (instrs)
            (inj-ecs-tcomp (tcomp-ext1-ρ f m shape-fn instrs) tc-counter-data)))]
        [(tcomp-reshape s tp)
         (->ecs
          (ecs-expr tp counter)
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
    (gr-expr t)))

(define gr-expr
  (λ (t)
    (match t
      [(tpromise tc _) (gr-expr tc)]
      [v #:when (number? v) v]
      [(tcomp) (gr-tcomp t)])))

(define gr-tcomp
  (λ (tc)
    (match tc
      [(tcomp-list->tensor lst)
       (let ((instrs-list (map gr-expr lst)))
         `(flat:list->tensor (list ,@instrs-list)))]
      [(tcomp-build-tensor s f)
       (flat:build-tensor s f)]
      [(tcomp-tref tp i)
       (let ((instrs (gr-expr tp))
             (i-instrs (gr-expr i)))
         `(flat:tref ,instrs ,i-instrs))]
      [(tcomp-trefs tp b)
       (let ((instrs (gr-expr tp))
             (b-instrs (gr-expr b)))
         `(rt:trefs ,instrs ,b-instrs))]
      [(tcomp-ext2-∇ fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
       (let ((t0-instrs (gr-expr tp-t0))
             (t1-instrs (gr-expr tp-t1))
             (z-instrs (gr-expr tp-z)))
         `(let* ([b (if (zero? ,i) ,out0 ,out1)]
                 [v (ext2-∇-result-res b)])
            (cond
              ((eqv? v 'uncalculated)
               (ext2-∇-forcer ,fᵈ ,r0 ,r1 ,shape-fn
                              ,t0-instrs ,t1-instrs
                              ,z-instrs ,out0 ,out1)
               (ext2-∇-result-res b))
              (else v))))]
      [(tcomp-ext1-∇ tp zp f m shape-fn)
       (let ((t-instrs (gr-expr tp))
             (z-instrs (gr-expr zp)))
         `(scalarize
           (flat-ext1-∇ ,f ,m ,shape-fn
                        (ensure-flat ,t-instrs)
                        (ensure-flat ,z-instrs))))]
      [(tcomp-ext2-ρ-scalar f tp-t tp-u)
       (let ((t-instrs (gr-expr tp-t))
             (u-instrs (gr-expr tp-u)))
         `(,f ,t-instrs ,u-instrs))]
      [(tcomp-ext2-ρ tp-t tp-u f m n shape-fn)
       (let ((t-instrs (gr-expr tp-t))
             (u-instrs (gr-expr tp-u)))
         `(scalarize
           (flat-ext2-ρ ,f ,m ,n ,shape-fn
                        (ensure-flat ,t-instrs)
                        (ensure-flat ,u-instrs))))]
      [(tcomp-ext1-ρ-scalar f tp)
       (let ((instrs (gr-expr tp)))
         `(,f ,instrs))]
      [(tcomp-ext1-ρ f m shape-fn tp)
       (let ((instrs (gr-expr tp)))
         `(scalarize
           (flat-ext1-ρ ,f ,m ,shape-fn
                        (ensure-flat ,instrs))))]
      [(tcomp-reshape s tp)
       (let ((instrs (gr-expr tp)))
         `(flat ',s
                (flat-store ,instrs)
                (flat-offset ,instrs)))]
      [(tcomp-let lhs rhs body)
       (let ((rhs-instrs (gr-expr rhs))
             (body-instrs (gr-expr body)))
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
(define compile-tensor
  (let ((cache (make-hash)))
    (λ (t)
      (display-compiler-trace 'Source-Tensor t)
      (let-values (((eds-instrs ds) (extract-data-segment t)))
        (display-compiler-trace 'Extract-Data-Segment-data ds)
        (display-compiler-trace 'Extract-Data-Segment-instructions eds-instrs)
        (let ((signature (generate-signature eds-instrs)))
          (display-compiler-trace 'Generate-Signature signature)
          (cond
            ;; TODO: Uncomment this to reenable caching
            (#f #;(hash-has-key? cache signature)
             (let ((compiled (hash-ref cache signature)))
               (display-compiler-trace 'Cache-Hit compiled)
               (values compiled ds)))
            (else
             (let ((counter (count-references eds-instrs)))
               (display-compiler-trace 'Count-References counter)
               (let ((extracted (extract-common-subexpressions eds-instrs counter)))
                 (display-compiler-trace 'Extract-Common-Subexpressions extracted)
                 (let ((rkt (generate-racket extracted)))
                   (display-compiler-trace 'Generate-Racket rkt)
                   (hash-set! cache signature rkt)
                   (values rkt ds)))))))))))

;;TODO: update this for new compiler passes
(define compile-tensor/checks
  (λ (t)
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
      (make-instrs instrs env))))

(include "test/test-c3-compiler.rkt")
(provide get-compiled compile-tensor compile-tensor/checks print-compiler?)
