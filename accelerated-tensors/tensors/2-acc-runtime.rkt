#lang racket

(require ffi/cvector
         ffi/unsafe
         opencl/c
         string-interpolation
         file/xxhash32
         "0-vectors.rkt"
         "../../impl-loader.rkt"
         "ext2-strides.rkt")


;; TODO: Implement MNIST as an example along with iris and morse

(define local-work-size (make-parameter #f))
(define xxh32-ctx (make-xxh32))

(define context
  (let ([context #f])
    (λ ()
      (or context
          (begin
            (set! context (clCreateContext #f (cvector->vector (devices))))
            context)))))
(define command-queue
  (let ([command-queue #f])
    (λ ()
      (or command-queue
          (begin
            (set! command-queue (clCreateCommandQueue (context) (device) '()))
            command-queue)))))

(define old-exit-handler (exit-handler))
(exit-handler
 (λ (v)
   (when (command-queue)
     (clReleaseCommandQueue (command-queue)))
   (when (context)
     (clReleaseContext (context)))
   (old-exit-handler v)))

(define platform
  (let ([platform #f])
    (lambda ()
      (or platform
          (begin
            (set! platform (cvector-ref (clGetPlatformIDs:vector) 0))
            platform)))))
(define devices
  (let ([devices #f])
    (lambda ()
      (or devices
          (begin
            (set! devices (clGetDeviceIDs:vector (platform) 'CL_DEVICE_TYPE_GPU))
            devices)))))
(define device
  (let ([device #f])
    (lambda ()
      (or device
          (begin
            (set! device (cvector-ref (devices) 0))
            device)))))

(define (cvector->vector cv)
  (build-vector (cvector-length cv)
                (curry cvector-ref cv)))

;; callback function to be used for debugging clBuildProgram if we expose that
;; parameter in the opencl/c library source code.
(define print-cl-build-log
  (λ (program _)
    (when (debug-kernel?)
      (printf "Program Source:~n~a~n"
              (clGetProgramInfo:generic program 'CL_PROGRAM_SOURCE))
      (printf "Build status:~a~n"
              (clGetProgramBuildInfo:generic program (device)
                                             'CL_PROGRAM_BUILD_STATUS))
      (printf "Build log:~a~n"
              (clGetProgramBuildInfo:generic program (device)
                                             'CL_PROGRAM_BUILD_LOG)))))

(define (binary-expr rator rand1 rand2)
  (string-append "(" rand1 " " rator " " rand2 ")"))

(define idx-exprs
  (λ (strides i0 i1)
    (λ (out-i)
      (for/fold ([i0 (number->string i0)]
                 [i1 (number->string i1)]
                 [x out-i] #:result (values i0 i1))
                ([stride (strides-strides strides)])
        (let ((stride-out (number->string (vector-ref stride 0)))
              (stride0 (number->string (vector-ref stride 1)))
              (stride1 (number->string (vector-ref stride 2))))
          (let ((idx (binary-expr "/" x stride-out))
                (next-x (binary-expr "%" x stride-out)))
            (values (binary-expr "+" i0 (binary-expr "*" idx stride0))
                    (binary-expr "+" i1 (binary-expr "*" idx stride1))
                    next-x)))))))

(define idx-exprs-inv
  (λ (strides i-out repeats0 repeats1 s-out)
    (λ (i0-var-str i1-var-str i-rep-var-str)
      (let ((gen-expr
             (λ (i-in-var-str stride-i repeats)
               (for/fold ([i-out (number->string i-out)]
                          [dividend-rep i-rep-var-str]
                          [predivisor-rep repeats]
                          [x i-in-var-str] #:result i-out)
                         ([desc-out s-out] ;; s-out == (append descents-out sf-out)
                          [stride (strides-strides strides)]) ;; (len strides) == (len descents-out)
                 (let ((stride-out (vector-ref stride 0))
                       (stride-in (vector-ref stride stride-i)))
                   (cond
                     ((zero? stride-in)
                      (let* ((divisor-rep (quotient predivisor-rep desc-out))
                             (divisor-rep-str (number->string divisor-rep))
                             (scaling (binary-expr "/" dividend-rep divisor-rep-str))
                             (next-dividend (binary-expr "%"
                                                         dividend-rep
                                                         divisor-rep-str)))
                        (values (binary-expr "+" i-out
                                             (binary-expr "*"
                                                          scaling
                                                          (number->string
                                                           stride-out)))
                                next-dividend
                                divisor-rep
                                x)))
                     (else
                      (let ((stride-in-str (number->string stride-in)))
                        (let ((idx (binary-expr "/" x stride-in-str))
                              (next-x (binary-expr "%" x stride-in-str)))
                          (values (binary-expr "+" i-out
                                               (binary-expr "*" idx
                                                            (number->string
                                                             stride-out)))
                                  dividend-rep
                                  predivisor-rep
                                  next-x))))))))))
        (values (gen-expr i0-var-str 1 repeats0)
                (gen-expr i1-var-str 2 repeats1))))))

(define calc-repeats
  (λ (s0 s1 r0 r1 s-out r-out)
    (define size-rep0 (apply * (drop-right s0 r0)))
    (define size-rep1 (apply * (drop-right s1 r1)))
    (define size-rep-out (apply * (drop-right s-out r-out)))
    (values (/ size-rep-out size-rep0)
            (/ size-rep-out size-rep1))))

(define kernel-name
  (lambda (fn)
    "kernel_@{(~a (eq-hash-code fn))}"))

(define (ext1-ρ-kernel/name prim1-ρ-f prim-sign)
  (values
   #<<EOF
__kernel void @{prim-sign} (__global float* v0,
                      int stride0,
                      __global float* v_out,
                      int stride_out)
{

    int i_out = get_global_id(0) * stride_out;
    // offset is handled by the platform API
    int i0 = (i_out / stride_out) * stride0;

@{(prim1-ρ-f "v0" "i0" "stride0" "v_out" "i_out" "stride_out")}

}
EOF
   prim-sign))

(define (run-prim1-ρ! kernel-code ker-name
                      v0 off0 size0 stride0
                      v-out size-out stride-out)
  (when (debug-kernel?)
    (printf "Number of GPU threads: ~a~n" (/ size-out stride-out))
    (printf "Input size: ~a~n" size0)
    (printf "Output size: ~a~n" size-out)
    (printf "Kernel Code:~n~a~n" kernel-code))
  (let* ([buf0 #f]
         [buf-out #f]
         [program #f]
         [kernel #f]
         [event #f])
    (dynamic-wind
     (λ ()
       (set! buf0 (clCreateBuffer (context)
                                  '(CL_MEM_USE_HOST_PTR CL_MEM_READ_ONLY)
                                  (* (ctype-sizeof _cl_float)
                                     size0)
                                  (vref-cpointer v0 off0)))
       (set! buf-out (clCreateBuffer (context) 'CL_MEM_WRITE_ONLY
                                     (* (ctype-sizeof _cl_float)
                                        size-out)
                                     #f))
       (set! program (clCreateProgramWithSource (context)
                                                (make-vector
                                                 1
                                                 (string->bytes/utf-8
                                                  kernel-code))))
       (clBuildProgram program (vector (device)) (make-bytes 0))
       (set! kernel (clCreateKernel program (string->bytes/utf-8 ker-name)))
       (clSetKernelArg:_cl_mem kernel 0 buf0)
       (clSetKernelArg:_cl_int kernel 1 stride0)
       (clSetKernelArg:_cl_mem kernel 2 buf-out)
       (clSetKernelArg:_cl_int kernel 3 stride-out))
     (λ ()
       ;;TODO: Try using the local-work-size argument
       (set! event (clEnqueueNDRangeKernel (command-queue) kernel 1
                                           (make-vector 1 (/ size-out stride-out))
                                           (or (local-work-size) (make-vector 0))
                                           (make-vector 0)))
       (set! event (clEnqueueReadBuffer (command-queue) buf-out 'CL_TRUE 0
                                        (* (ctype-sizeof _cl_float)
                                           size-out)
                                        (vec->cpointer v-out) (vector event))))
     (λ ()
       (when kernel
         (clReleaseKernel kernel))
       (when program
         (clReleaseProgram program))
       (when buf-out
         (clReleaseMemObject buf-out))
       (when buf0
         (clReleaseMemObject buf0))))))

(define functional->preallocated-1-ρ-acc
  (λ (f-acc base-shape out-shape)
    (unless (and (null? base-shape) (null? out-shape))
      (error 'ρ1-functional-non-scalar-acc
             (string-append "Accelerated functional primitives can only accept and"
                            " return scalars, so try defining a"
                            " preallocated primitive instead."
                            " Input and output shape found: ~a ~a")
             base-shape out-shape))
    (λ (v0 i0 stride0 v-out i-out stride-out)
      (let ((a "@{v0}[@{i0}]"))
        #<<EOF
    @{v-out}[@{i-out}] = (@{(f-acc a)});
EOF
        ))))

(define (ext1-∇-kernel/name prim1-∇-f prim-sign)
  (values
   #<<EOF
__kernel void @{prim-sign} (__global float* g0,
                      __global float* v0,
                      int stride0,
                      __global float* vz,
                      int stridez)
{

    int iz = get_global_id(0) * stridez;
    // offset is handled by the platform API
    int i0 = 0 + (iz / stridez) * stride0;

@{(prim1-∇-f "g0" "v0" "i0" "stride0"
                  "vz" "iz" "stridez")}
}
EOF
   prim-sign))

(define (run-prim1-∇! kernel-code ker-name g0
                      v0 off0 size0 stride0
                      vz offz size-z stride-z)
  (when (debug-kernel?)
    (printf "Number of GPU threads: ~a~n" (/ size-z stride-z))
    (printf "Input size: ~a~n" size0)
    (printf "Output size: ~a~n" size-z)
    (printf "Kernel Code:~n~a~n" kernel-code))
  (let* ([buf0 #f]
         [buf-z #f]
         [buf-g #f]
         [program #f]
         [kernel #f]
         [event #f])
    (dynamic-wind
     (λ ()
       (set! buf0 (clCreateBuffer (context)
                                  '(CL_MEM_USE_HOST_PTR CL_MEM_READ_ONLY)
                                  (* (ctype-sizeof _cl_float)
                                     size0)
                                  (vref-cpointer v0 off0)))
       (set! buf-z (clCreateBuffer (context)
                                   '(CL_MEM_USE_HOST_PTR CL_MEM_READ_ONLY)
                                   (* (ctype-sizeof _cl_float)
                                      size-z)
                                   (vref-cpointer vz offz)))
       (set! buf-g (clCreateBuffer (context) 'CL_MEM_WRITE_ONLY
                                   (* (ctype-sizeof _cl_float)
                                      size0)
                                   #f))
       (set! program (clCreateProgramWithSource (context)
                                                (make-vector
                                                 1
                                                 (string->bytes/utf-8
                                                  kernel-code))))
       (clBuildProgram program (vector (device)) (make-bytes 0))
       (set! kernel (clCreateKernel program (string->bytes/utf-8 ker-name)))
       (clSetKernelArg:_cl_mem kernel 0 buf-g)
       (clSetKernelArg:_cl_mem kernel 1 buf0)
       (clSetKernelArg:_cl_int kernel 2 stride0)
       (clSetKernelArg:_cl_mem kernel 3 buf-z)
       (clSetKernelArg:_cl_int kernel 4 stride-z))
     (λ ()
       (set! event (clEnqueueNDRangeKernel (command-queue) kernel 1
                                           (make-vector 1 (/ size-z stride-z))
                                           (or (local-work-size) (make-vector 0))
                                           (make-vector 0)))
       (set! event (clEnqueueReadBuffer (command-queue) buf-g 'CL_TRUE 0
                                        (* (ctype-sizeof _cl_float)
                                           size0)
                                        (vec->cpointer g0) (vector event))))
     (λ ()
       (when kernel
         (clReleaseKernel kernel))
       (when program
         (clReleaseProgram program))
       (when buf-g
         (clReleaseMemObject buf-g))
       (when buf-z
         (clReleaseMemObject buf-z))
       (when buf0
         (clReleaseMemObject buf0))))
    ;))
    ))

(define functional->preallocated-1-∇-acc
  (λ (f-acc base-shape out-shape)
    (unless (and (null? base-shape) (null? out-shape))
      (error '∇1-functional-non-scalar-acc
             (string-append "Accelerated functional primitives can only accept and"
                            " return scalars, so try defining a"
                            " preallocated primitive instead."
                            " Input and output shape found: ~a ~a")
              base-shape out-shape))
    (λ (g0 v0 i0 stride0 vz iz stride-z)
      (let ((z "@{vz}[@{iz}]")
            (a "@{v0}[@{i0}]"))
        #<<EOF
    @{g0}[@{i0}] += (@{(f-acc a z)});
EOF
        ))))

(define (ext2-ρ-kernel-name prim-sign strides)
  (format "~a~a" prim-sign (strides-signature strides)))

(define ext2-ρ-kernel/name
  (let ((cache (make-hash)))
    (λ (prim2-ρ-f prim-sign strides)
      (let ((kernel-name (ext2-ρ-kernel-name prim-sign strides)))
        (cond
          ((hash-has-key? cache kernel-name)
           (values (hash-ref cache kernel-name) kernel-name))
          (else
           (let*-values (((generate-idxs) (idx-exprs strides 0 0))
                         ((i0-expr i1-expr) (generate-idxs "i_out")))
             (define kernel-code
               #<<EOF
__kernel void @{kernel-name} (__global float* v0,
                      int stride0,
                      __global float* v1,
                      int stride1,
                      __global float* v_out,
                      int stride_out)
{
    int i_out = get_global_id(0) * stride_out;
    int i0 = @{i0-expr};
    int i1 = @{i1-expr};

@{(prim2-ρ-f "v0" "i0" "stride0"
             "v1" "i1" "stride1"
             "v_out" "i_out" "stride_out")}
}
EOF
               )
             (hash-set! cache kernel-name kernel-code)
             (values
              kernel-code
              kernel-name))))))))

(define (run-prim2-ρ! kernel-code ker-name
                      v0 off0 size0 stride0
                      v1 off1 size1 stride1
                      v-out size-out stride-out)
  (when (debug-kernel?)
    (printf "Number of GPU threads: ~a~n" (/ size-out stride-out))
    (printf "Input 0 size: ~a~n" size0)
    (printf "Input 1 size: ~a~n" size1)
    (printf "Output size: ~a~n" size-out)
    (printf "Kernel Code:~n~a~n" kernel-code))
  (let* ([buf0 #f]
         [buf1 #f]
         [buf-out #f]
         [program #f]
         [kernel #f]
         [event #f])
    (dynamic-wind
     (λ ()
       (set! buf0 (clCreateBuffer (context)
                                  '(CL_MEM_USE_HOST_PTR CL_MEM_READ_ONLY)
                                  (* (ctype-sizeof _cl_float)
                                     size0)
                                  (vref-cpointer v0 off0)))
       (set! buf1 (clCreateBuffer (context)
                                  '(CL_MEM_USE_HOST_PTR CL_MEM_READ_ONLY)
                                  (* (ctype-sizeof _cl_float)
                                     size1)
                                  (vref-cpointer v1 off1)))
       (set! buf-out (clCreateBuffer (context) 'CL_MEM_WRITE_ONLY
                                     (* (ctype-sizeof _cl_float)
                                        size-out)
                                     #f))
       (set! program (clCreateProgramWithSource
                      (context)
                      (make-vector
                       1
                       (string->bytes/utf-8 kernel-code))))
       (clBuildProgram program (vector (device)) (make-bytes 0))
       (set! kernel (clCreateKernel program (string->bytes/utf-8 ker-name)))
       (clSetKernelArg:_cl_mem kernel 0 buf0)
       (clSetKernelArg:_cl_int kernel 1 stride0)
       (clSetKernelArg:_cl_mem kernel 2 buf1)
       (clSetKernelArg:_cl_int kernel 3 stride1)
       (clSetKernelArg:_cl_mem kernel 4 buf-out)
       (clSetKernelArg:_cl_int kernel 5 stride-out))
     (λ ()
       (set! event (clEnqueueNDRangeKernel (command-queue) kernel 1
                                           (make-vector 1 (/ size-out stride-out))
                                           (or (local-work-size) (make-vector 0))
                                           (make-vector 0)))
       (set! event (clEnqueueReadBuffer (command-queue) buf-out 'CL_TRUE 0
                                        (* (ctype-sizeof _cl_float)
                                           size-out)
                                        (vec->cpointer v-out) (vector event))))
     (λ ()
       (when kernel
         (clReleaseKernel kernel))
       (when program
         (clReleaseProgram program))
       (when buf-out
         (clReleaseMemObject buf-out))
       (when buf1
         (clReleaseMemObject buf1))
       (when buf0
         (clReleaseMemObject buf0))))))

(define functional->preallocated-2-ρ-acc
  (λ (f-acc t-shape u-shape out-shape)
    (unless (and (null? t-shape) (null? u-shape) (null? out-shape))
      (error 'ρ2-functional-non-scalar-acc
             (string-append "Accelerated functional primitives can only accept and"
                            " return scalars, so try defining a"
                            " preallocated primitive instead."
                            " Input 1, input 2 and output shape found: ~a ~a ~a")
              t-shape u-shape out-shape))
    (λ (v0 i0 stride0 v1 i1 stride1 v-out i-out stride-out)
      (let ((a "@{v0}[@{i0}]")
            (b "@{v1}[@{i1}]"))
        #<<EOF
    @{v-out}[@{i-out}] = (@{(f-acc a b)});
EOF
        ))))

(define (ext2-∇-kernel-name prim-sign strides
                            s0 s1 r0 r1 s-out r-out)
  (xxh32-reset! xxh32-ctx 0)
  (xxh32-update! xxh32-ctx (string->bytes/utf-8 (strides-signature strides)))
  (xxh32-update!
   xxh32-ctx
   (bytes-append (apply bytes-append
                        (map (λ (x) (integer->integer-bytes x 4 #f)) s0))
                 (apply bytes-append
                        (map (λ (x) (integer->integer-bytes x 4 #f)) s1))
                 (integer->integer-bytes r0 1 #f)
                 (integer->integer-bytes r1 1 #f)
                 (apply bytes-append
                        (map (λ (x) (integer->integer-bytes x 4 #f)) s-out))
                 (integer->integer-bytes r-out 1 #f)))
  (define params-hash (xxh32-digest xxh32-ctx))
  (format "~a~a" prim-sign params-hash))


(define ext2-∇-kernel/name
  (let ((cache (make-hash)))
    (λ (prim2-∇-f prim-sign strides
                  s0 s1 r0 r1 s-out r-out)
      (let ((kernel-name (ext2-∇-kernel-name prim-sign strides
                                             s0 s1 r0 r1 s-out r-out)))
        (cond
          ((hash-has-key? cache kernel-name)
           (values (hash-ref cache kernel-name) kernel-name))
          (else
           (let*-values (((prim-effect0 prim-effect1) (prim2-∇-f "g"
                                                                 "v0" "i0" "stride0"
                                                                 "v1" "i1" "stride1"
                                                                 "vz" "iz" "stride_z"))
                         ((repeats0 repeats1) (calc-repeats s0 s1 r0 r1 s-out r-out))
                         ((generate-idxs) (idx-exprs strides 0 0))
                         ((generate-idxs-inv) (idx-exprs-inv strides 0
                                                             repeats0 repeats1 s-out))
                         ((i0-expr i1-expr) (generate-idxs "iz"))
                         ((iz-expr0 iz-expr1) (generate-idxs-inv "i0" "i1" "i_rep")))
             (define kernel-code
              #<<EOF
__kernel void @{kernel-name} (__global float* g0,
                      __global float* g1,
                      __global float* v0,
                      int stride0,
                      int size0,
                      __global float* v1,
                      int stride1,
                      int size1,
                      __global float* vz,
                      int stride_z)
{
    int g_id = get_global_id(0);
    int i0_g = g_id * stride0;
    int i1_g = g_id * stride1;
    __global float *g;
    int i0, i1, iz;

    if (i0_g < size0) {
        g = g0;
        i0 = i0_g;
        for(int i_rep=0; i_rep<@{repeats0}; i_rep++) {
            iz = @{iz-expr0};
            i1 = @{i1-expr};

@{prim-effect0}
        }
    }

    if (i1_g < size1) {
        g = g1;
        i1 = i1_g;
        for(int i_rep=0; i_rep<@{repeats1}; i_rep++) {
            iz = @{iz-expr1};
            i0 = @{i0-expr};

@{prim-effect1}
        }
    }
}
EOF
               )
             (hash-set! cache kernel-name kernel-code)
             (values
              kernel-code
              kernel-name))))))))

(define (run-prim2-∇! kernel-code ker-name g0 g1
                      v0 off0 size0 stride0
                      v1 off1 size1 stride1
                      vz offz size-z stride-z)
  (when (debug-kernel?)
    (printf "Number of GPU threads: ~a~n" (max (/ size0 stride0)
                                             (/ size1 stride1)))
    (printf "Input 0 size: ~a~n" size0)
    (printf "Input 1 size: ~a~n" size1)
    (printf "Output size: ~a~n" size-z)
    (printf "Kernel Code:~n~a~n" kernel-code))
  (let* ([global-work-size (max (/ size0 stride0)
                                (/ size1 stride1))]
         [buf0 #f]
         [buf1 #f]
         [buf-z #f]
         [buf-g0 #f]
         [buf-g1 #f]
         [program #f]
         [kernel #f]
         [event #f])
    (dynamic-wind
     (λ ()
       (set! buf0 (clCreateBuffer (context)
                                  '(CL_MEM_USE_HOST_PTR CL_MEM_READ_ONLY)
                                  (* (ctype-sizeof _cl_float)
                                     size0)
                                  (vref-cpointer v0 off0)))
       (set! buf1 (clCreateBuffer (context)
                                  '(CL_MEM_USE_HOST_PTR CL_MEM_READ_ONLY)
                                  (* (ctype-sizeof _cl_float)
                                     size1)
                                  (vref-cpointer v1 off1)))
       (set! buf-z (clCreateBuffer (context)
                                   '(CL_MEM_USE_HOST_PTR CL_MEM_READ_ONLY)
                                   (* (ctype-sizeof _cl_float)
                                      size-z)
                                   (vref-cpointer vz offz)))
       (set! buf-g0 (clCreateBuffer (context) 'CL_MEM_WRITE_ONLY
                                    (* (ctype-sizeof _cl_float)
                                       size0)
                                    #f))
       (set! buf-g1 (clCreateBuffer (context) 'CL_MEM_WRITE_ONLY
                                    (* (ctype-sizeof _cl_float)
                                       size1)
                                    #f))
       (set! program (clCreateProgramWithSource
                      (context)
                      (make-vector 1 (string->bytes/utf-8 kernel-code))))
       (clBuildProgram program (vector (device)) (make-bytes 0))
       (set! kernel (clCreateKernel program (string->bytes/utf-8 ker-name)))
       (clSetKernelArg:_cl_mem kernel 0 buf-g0)
       (clSetKernelArg:_cl_mem kernel 1 buf-g1)
       (clSetKernelArg:_cl_mem kernel 2 buf0)
       (clSetKernelArg:_cl_int kernel 3 stride0)
       (clSetKernelArg:_cl_int kernel 4 size0)
       (clSetKernelArg:_cl_mem kernel 5 buf1)
       (clSetKernelArg:_cl_int kernel 6 stride1)
       (clSetKernelArg:_cl_int kernel 7 size1)
       (clSetKernelArg:_cl_mem kernel 8 buf-z)
       (clSetKernelArg:_cl_int kernel 9 stride-z))
     (λ ()
       (set! event (clEnqueueNDRangeKernel (command-queue) kernel 1
                                           (make-vector 1 global-work-size)
                                           (or (local-work-size) (make-vector 0))
                                           (make-vector 0)))
       (set! event (clEnqueueReadBuffer (command-queue) buf-g0 'CL_TRUE 0
                                        (* (ctype-sizeof _cl_float)
                                           size0)
                                        (vec->cpointer g0) (vector event)))
       (set! event (clEnqueueReadBuffer (command-queue) buf-g1 'CL_TRUE 0
                                        (* (ctype-sizeof _cl_float)
                                           size1)
                                        (vec->cpointer g1) (vector event))))
     (λ ()
       (when kernel
         (clReleaseKernel kernel))
       (when program
         (clReleaseProgram program))
       (when buf-g1
         (clReleaseMemObject buf-g1))
       (when buf-g0
         (clReleaseMemObject buf-g0))
       (when buf-z
         (clReleaseMemObject buf-z))
       (when buf1
         (clReleaseMemObject buf1))
       (when buf0
         (clReleaseMemObject buf0))))))

(define functional->preallocated-2-∇-acc
  (λ (f-acc t-shape u-shape out-shape)
    (unless (and (null? t-shape) (null? u-shape) (null? out-shape))
      (error '∇2-functional-non-scalar-acc
             (string-append "Accelerated functional primitives can only accept and"
                            " return scalars, so try defining a"
                            " preallocated primitive instead."
                            " Input 1, input 2 and output shape found: ~a ~a ~a")
              t-shape u-shape out-shape))
    (λ (g v0 i0 stride0 v1 i1 stride1 vz iz stride-z)
      (let ((z "@{vz}[@{iz}]")
            (a "@{v0}[@{i0}]")
            (b "@{v1}[@{i1}]"))
        (let-values (((da db) (f-acc a b z)))
          (values
           #<<EOF
    @{g}[@{i0}] += (@{da});
EOF

           #<<EOF
    @{g}[@{i1}] += (@{db});
EOF
           ))))))

(include "test/test-2-acc-runtime.rkt")

(provide run-prim1-ρ! functional->preallocated-1-ρ-acc ext1-ρ-kernel/name
         run-prim1-∇! functional->preallocated-1-∇-acc ext1-∇-kernel/name
         run-prim2-ρ! functional->preallocated-2-ρ-acc ext2-ρ-kernel/name
         run-prim2-∇! functional->preallocated-2-∇-acc ext2-∇-kernel/name
         kernel-name)
