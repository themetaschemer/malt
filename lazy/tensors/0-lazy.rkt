#lang racket
(require "../../flat-tensors/ext-impl.rkt")
(require (prefix-in flat: "../../flat-tensors/tensors.rkt"))


;; tensor computations
(struct tcomp ())
#;
(: lst (U (Listof tpromise) (Listof Number)))
(struct tcomp-list->tensor tcomp (lst) #:transparent)
#;
(: s (Listof Natural)) ;; non-empty
#;
(: f (-> (Listof Natural) Number))
(struct tcomp-build-tensor tcomp (s f) #:transparent)
#;
(: tp tpromise)
#;
(: i Natural)
(struct tcomp-tref tcomp (tp i) #:transparent)
#;
(: tp tpromise)
#;
(: i (Listof Natural))
(struct tcomp-trefs tcomp (tp b) #:transparent)
#;
(: fᵈ (U (-> Number Number (Values Number Number))
         (-> (Vector Number) Natural (Listof Natural)
             (Vector Number) Natural (Listof Natural)
             (Vector Number) Natural (Listof Natural))))
(struct tcomp-ext2-∇ tcomp (fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
  #:transparent)
(struct tcomp-ext1-∇-prealloc tcomp (tp zp f m shape-fn) #:transparent)
(struct tcomp-ext1-∇ tcomp (tp zp f m shape-fn) #:transparent)
(struct tcomp-ext2-ρ-scalar tcomp (f tp-t tp-u) #:transparent)
(struct tcomp-ext2-ρ-prealloc tcomp (tp-t tp-u f m n shape-fn) #:transparent)
(struct tcomp-ext2-ρ tcomp (tp-t tp-u f m n shape-fn) #:transparent)
(struct tcomp-ext1-ρ-scalar tcomp (f tp) #:transparent)
(struct tcomp-ext1-ρ-prealloc tcomp (f m shape-fn tp) #:transparent)
(struct tcomp-ext1-ρ tcomp (f m shape-fn tp) #:transparent)
(struct tcomp-reshape tcomp (s tp) #:transparent)
#;
(: args (U (Listof tpromise) (Listof Number)))
(struct tcomp-tensor tcomp (args) #:transparent)

(struct tpromise ((tensor #:mutable) shape)
  #:guard
  (λ (tensor shape name)
    (unless (or (flat? tensor) (tcomp? tensor))
      (error 'make-tpromise
             (string-append
              "First argument must be either a"
              " tcomp or a flat tensor. Got ~a")
             tensor))
    (unless ((listof positive-integer?) shape)
      (error 'make-tpromise
             (string-append
              "Second argument must be a list"
              " of positive integers. Got ~a")
             shape))
    (values tensor shape))
  #:transparent)

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

#;
(: ensure-flat (-> (U flat Number) flat))
(define ensure-flat
  (λ (v)
    (cond
      ((scalar? v) (flat '() (vec v) 0))
      (else v))))

(define-namespace-anchor a)

(define run-instrs
  (lambda (instrs)
    (let ([env (namespace-anchor->namespace a)])
      (eval instrs env))))

(define force/eval
  (lambda (tp (print? #f))
    (when print?
      (printf "~n####PP tensor: ")
      (pretty-print tp))
    (match tp
      [(tpromise t _)
       #:when (or (flat? t) (scalar? t) (tcomp? t))

       (let-values (((instrs locals env)
                     (run-compiler (compile-expr t (count-references t (hasheq)))
                                   '()
                                   '())))
         (let ((res (run-instrs instrs locals env)))
           (set-tpromise-tensor! tp res)
           res))]
      ;; NOTE: This case runs when we use tp-scalarize to turn
      ;; the tensor to a scalar
      (_ tp))))

(struct counter-data (binding-name
                      ref-count
                      (compiled? #:mutable #:auto))
  #:transparent)

;; Count references so that the tcomp AST nodes that refer to the same memory
;; location i.e. common AST nodes get extracted by let-binding them in the
;; compiled output racket code.
(define count-tcomp-references
  (λ (tc counter)
    (match-let (((counter-data tc-binding-name tc-ref-count _)
                 (hash-ref counter tc
                           (λ ()
                             (counter-data (gensym 'local) 0)))))
      (let ((counter^ (hash-set counter tc
                                (counter-data tc-binding-name
                                            (add1 tc-ref-count)))))
        (match tc
          [(tcomp-list->tensor lst)
           (for/fold
            ((counter^^ counter^))
            ((l lst))
             (count-references l counter^^))]
          [(tcomp-build-tensor s f) counter^]
          [(tcomp-tref tp i)
           (count-references tp counter^)]
          [(tcomp-trefs tp b)
           (count-references tp counter^)]
          [(tcomp-ext2-∇ fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
           (count-references
            tp-z
            (count-references
             tp-t1
             (count-references tp-t0 counter^)))]
          [(tcomp-ext1-∇ tp zp f m shape-fn)
           (count-references
            zp
            (count-references tp counter^))]
          [(tcomp-ext2-ρ-scalar f tp-t tp-u)
           (count-references
            tp-u
            (count-references tp-t counter^))]
          [(tcomp-ext2-ρ tp-t tp-u f m n shape-fn)
           (count-references
            tp-u
            (count-references tp-t counter^))]
          [(tcomp-ext1-ρ-scalar f tp)
           (count-references tp counter^)]
          [(tcomp-ext1-ρ f m shape-fn tp)
           (count-references tp counter^)]
          [(tcomp-reshape s tp)
           (count-references tp counter^)])))))

(define count-references
  (λ (t counter)
    (match t
      ((tpromise tc _)
       (count-references tc counter))
      ((tcomp) (count-tcomp-references t counter))
      (_ counter))))

;; TODO: Add tcomp nodes for let and var so that common refs to tcomp can become
;; tcomp-vars and be bound in a tcomp-let.
;;
;; TODO: Add another intermediate pass that generates tcomp-let and tcomp-var
;; nodes in such a way that it converts the abstract syntax graph to a abstract
;; syntax tree.
#|
(tcomp ..) =>
 (tcomp-let ((var (tcomp ...))) (tcomp ... var ....))

(tpromise (tcomp ... (tcomp-var name) ...))
|#

(define extend-env
  (λ (k v env)
    `((,k . ,v) . ,env)))
(define extend-locals extend-env)

(define exists-in-env?
  (λ (ft env)
    (match env
      ('() #f)
      (`((,k . ,v) . ,_) #:when (eq? ft v) k)
      (`(,_ . ,rest-env) (exists-in-env? ft rest-env)))))

;; (: run-compiler (All (Instrs Env) (-> Env (Values Instrs Env))))
(struct Compiler (run-compiler) #:transparent)

(define run-compiler
  (λ (c locals env)
    ((Compiler-run-compiler c) locals env)))

(define inj-compiler-val
  (λ (v)
    (Compiler (λ (locals env) (values v locals env)))))

;; TODO: In the future scalars must be in the environment rather than the
;; compiled instrs. This will make the instruction signature fully independent
;; of the data in the environmnt.
(define inj-compiler-flat
  (λ (ft)
    (Compiler
      (λ (locals env)
        (cond
          ((exists-in-env? ft env) => (lambda (var) (values var locals env)))
          ((and (flat? ft) (null? (flat-shape ft)))
           (values ft locals env))
          (else
           (let ((new-var (gensym 'ft)))
             (values new-var locals (extend-env new-var ft env)))))))))

(define inj-compiler-instrs
  (λ (instrs cd)
    (match-let (((counter-data binding-var ref-count _) cd))
      (Compiler (λ (locals env)
                  (pretty-print instrs)
                  (cond
                    ((<= ref-count 1)
                     (println "ref <= 1")
                     (values instrs locals env))
                    ((assv binding-var locals)
                     (println "assv")
                     (values binding-var locals env))
                    (else
                     (println "else")
                     (values binding-var
                             (extend-locals binding-var instrs locals)
                             env))))))))

(define ->c
  (λ (c f)
    (Compiler (λ (locals env)
                (let-values (((instrs locals^ env^) (run-compiler c locals env)))
                  (run-compiler (f instrs) locals^ env^))))))


(define compile-tcomp
  (λ (tc counter)
    (let ((tc-counter-data
           (hash-ref counter tc
                     (λ ()
                       (counter-data (gensym 'illegal) 0)))))
      (match tc
        [(tcomp-list->tensor lst)
         (let ((instrs-list-compiler
                (for/foldr ((list-compiler (inj-compiler-val '())))
                  ((arg lst))
                  (->c
                   (compile-expr arg counter)
                   (λ (instrs)
                     (->c
                      list-compiler
                      (λ (instrs-list)
                        (inj-compiler-val (cons instrs instrs-list)))))))))
           (->c
            instrs-list-compiler
            (λ (instrs-list)
              (inj-compiler-instrs `(flat:list->tensor (list ,@instrs-list))
                                   tc-counter-data))))]
        [(tcomp-build-tensor s f)
         (inj-compiler-flat (flat:build-tensor s f))]
        [(tcomp-tref tp i)
         (->c
          (compile-expr tp counter)
          (λ (instrs)
            (inj-compiler-instrs `(flat:tref ,instrs ,i) tc-counter-data)))]
        [(tcomp-trefs tp b)
         (->c
          (compile-expr tp counter)
          (λ (instrs)
            (inj-compiler-instrs `(flat:trefs ,instrs ',b) tc-counter-data)))]
        [(tcomp-ext2-∇ fᵈ r0 r1 shape-fn tp-t0 tp-t1 tp-z out0 out1 i)
         (->c
          (compile-expr tp-t0 counter)
          (λ (t0-instrs)
            (->c
             (compile-expr tp-t1 counter)
             (λ (t1-instrs)
               (->c
                (compile-expr tp-z counter)
                (λ (z-instrs)
                  (inj-compiler-instrs
                   `(let* ([b (if (zero? ,i) ,out0 ,out1)]
                           [v (ext2-∇-result-res b)])
                      (cond
                        ((eqv? v 'uncalculated)
                         (ext2-∇-forcer ,fᵈ ,r0 ,r1 ,shape-fn
                                        ,t0-instrs ,t1-instrs
                                        ,z-instrs ,out0 ,out1)
                         (ext2-∇-result-res b))
                        (else v)))
                   tc-counter-data)))))))]
        [(tcomp-ext1-∇ tp zp f m shape-fn)
         (->c
          (compile-expr tp counter)
          (λ (t-instrs)
            (->c
             (compile-expr zp counter)
             (λ (z-instrs)
               (inj-compiler-instrs
                `(scalarize
                  (flat-ext1-∇ ,f ,m ,shape-fn
                               (ensure-flat ,t-instrs)
                               (ensure-flat ,z-instrs)))
                tc-counter-data)))))]
        [(tcomp-ext2-ρ-scalar f tp-t tp-u)
         (->c
          (compile-expr tp-t counter)
          (λ (t-instrs)
            (->c
             (compile-expr tp-u counter)
             (λ (u-instrs)
               (inj-compiler-instrs
                `(,f ,t-instrs ,u-instrs)
                tc-counter-data)))))]
        [(tcomp-ext2-ρ tp-t tp-u f m n shape-fn)
         (->c
          (compile-expr tp-t counter)
          (λ (t-instrs)
            (->c
             (compile-expr tp-u counter)
             (λ (u-instrs)
               (inj-compiler-instrs
                `(scalarize
                  (flat-ext2-ρ ,f ,m ,n ,shape-fn
                               (ensure-flat ,t-instrs)
                               (ensure-flat ,u-instrs)))
                tc-counter-data)))))]
        [(tcomp-ext1-ρ-scalar f tp)
         (->c
          (compile-expr tp counter)
          (λ (instrs)
            (inj-compiler-instrs `(,f ,instrs)
                                 tc-counter-data)))]
        [(tcomp-ext1-ρ f m shape-fn tp)
         (->c
          (compile-expr tp counter)
          (λ (instrs)
            (inj-compiler-instrs `(scalarize
                                   (flat-ext1-ρ ,f ,m ,shape-fn
                                                (ensure-flat ,instrs)))
                                 tc-counter-data)))]
        [(tcomp-reshape s tp)
         (->c
          (compile-expr tp counter)
          (λ (instrs)
            (inj-compiler-instrs `(flat ',s
                                        (flat-store ,instrs)
                                        (flat-offset ,instrs))
                                 tc-counter-data)))]))))

(define print-compiler? (make-parameter #f))
(define compile-expr
  (λ (tc counter)
    (when (print-compiler?)
      (pretty-print tc))
    (match tc
      [(tpromise tc _) (compile-expr tc counter)]
      [tc #:when (flat? tc)
          (inj-compiler-flat tc)]
      [tc #:when (or (pair? tc) (scalar? tc))
          (inj-compiler-val tc)]
      [(tcomp) (compile-tcomp tc counter)])))

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

(define list->tpromise
  (λ (lst)
    (cond
      [(null? lst)
       (error 'list->ltensor "No elements found")]
      [else
       (tpromise (tcomp-list->tensor lst)
                 `(,(length lst)
                   . ,(tp-shape
                       (car lst))))])))

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
          (tcomp-ext1-ρ-prealloc f m shape-fn tp)
          (merge-shapes
           (tp-shape tp)
           m
           (shape-fn
            (min-shape m (tp-shape tp)))))]
        [else
         (tpromise
          (tcomp-ext1-ρ f m shape-fn tp)
          (merge-shapes
           (tp-shape tp)
           m
           (shape-fn
            (min-shape m (tp-shape tp)))))]))))

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

;; We may have to replace tp-scalarize with scalarize from flat-tensors, because
;; the force/eval used  in its definition is undesirable.
(define tp-scalarize
  (λ (tp)
    (cond
      [(and (tpromise? tp) (null? (tpromise-shape tp)))
       (tp-scalarize (force/eval tp))]
      [(and (flat? tp) (null? (flat-shape tp)))
       (vref (flat-store tp) 0)]
      [else tp])))

(define scalar-shape
  (λ (s0 [s1 '()]) '()))

(define left-shape
  (λ (s0 s1) s0))

(define right-shape
  (λ (s0 s1) s1))

(define flat-function-maker2
  (λ (f m n)
    (cond
      ((and (zero? m) (zero? n))
       (λ (v0 i0 stride0 v1 i1 stride1
              v-out i-out stride-out)
         (vset! v-out i-out
                (f (vref v0 i0) (vref v1 i1)))))
      (else
       f))))

(define flat-function-maker1
  (λ (f m)
    (cond
      ((zero? m)
       (λ (v0 i0 stride0 v-out i-out stride-out)
         (vset! v-out i-out (f (vref v0 i0)))))
      (else f))))

(define tp-ext1-∇
  (λ (f m [shape-fn scalar-shape])
    (λ (tp zp)
      (cond
        ((number? tp) (f tp zp))
        ((flat:expects-preallocated? f)
         (tpromise
          (tcomp-ext1-∇-prealloc tp zp f m shape-fn)
          (tp-shape tp)))
        (else
         (tpromise
          (tcomp-ext1-∇ tp zp f m shape-fn)
          (tp-shape tp)))))))

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

(define ext2-∇-forcer
  (λ (fᵈ r0 r1 shape-fn t0 t1 z out0 out1)
    (let* ((f0 (ensure-flat t0))
           (f1 (ensure-flat t1))
           (fz (ensure-flat z))

           (s0 (flat-shape f0))
           (sf0 (min-shape r0 s0))
           (stride0 (flat:size-of sf0))

           (s1 (flat-shape t1))
           (sf1 (min-shape r1 s1))
           (stride1 (flat:size-of sf1))

           (sf-z (shape-fn sf0 sf1))
           (stride-z (flat:size-of sf-z))

           (v0 (flat-store f0))
           (v1 (flat-store f1))
           (vz (flat-store fz))

           (off0 (flat-offset f0))
           (off1 (flat-offset f1))
           (offz (flat-offset fz)))
      (ext2-shapes
       s0 s1 r0 r1 sf-z
       (λ (sz size-z q0 q1 strides)
         (let ((g0 (new-vec (flat:size-of
                             s0)
                            0.0))
               (g1 (new-vec (flat:size-of
                             s1)
                            0.0)))
           (for ([iz (in-range
                      0
                      size-z
                      stride-z)])
             (let-values (((i0 i1)
                           (idxs
                            strides
                            iz
                            off0
                            off1)))
               (fᵈ g0 g1 v0 i0
                   stride0
                   v1 i1
                   stride1
                   vz
                   (+ offz iz)
                   stride-z)))
           (set-ext2-∇-result-res! out0
                     (tp-scalarize (flat s0 g0 0)))
           (set-ext2-∇-result-res! out1
                     (tp-scalarize (flat s1 g1 0)))))))))

(struct ext2-∇-result (res) #:mutable #:transparent)
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

(define get-compiled
  (λ (t)
    (let-values (((instrs locals env)
                  (run-compiler
                   (compile-expr t
                                 (count-references t (hasheq)))
                   '() '())))
      (make-instrs instrs locals env))))
(define run-compiled
  (λ (t)
    (let-values (((instrs locals env)
                  (run-compiler
                   (compile-expr t
                                 (count-references t (hasheq)))
                   '() '())))
      (make-instrs instrs locals env))))
;; TODO: may have to remove call to force*1 & force*2 so that this can be
;; handled at compile time
(define force*1
  (λ (t f)
    (f (force/eval t))))

(define force*2
  (λ (ts f)
    (let-values (((t1 t2) (ts)))
      (f (force/eval t1) (force/eval t2)))))

(include "test/test-0-lazy.rkt")

(provide start-vector-manager vector-manager-report)

(provide (rename-out
          (flat:len len)
          (flat:ref ref)
          (flat:refr refr)))
(provide tensor
         force/eval
         tpromise?
         (rename-out
          (tp-scalarize scalarize)
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

(provide force*1 force*2)
