#lang racket

(require ffi/cvector
         ffi/unsafe
         opencl/c
         string-interpolation
         "0-vectors.rkt")


(define context (make-parameter #f))
(define command-queue (make-parameter #f))

(define (cvector->vector cv)
  (build-vector (cvector-length cv)
                (curry cvector-ref cv)))

(define (with-opencl th)
  (let* ([platform (cvector-ref (clGetPlatformIDs:vector) 0)]
         [devices (clGetDeviceIDs:vector platform 'CL_DEVICE_TYPE_GPU)]
         [device-idx 0]
         [device (cvector-ref devices device-idx)])
    (parameterize* ([context #f]
                    [command-queue #f])
      (dynamic-wind
       (λ ()
         (context (clCreateContext #f (cvector->vector devices)))
         (command-queue (clCreateCommandQueue (context) device '())))
       th
       (λ ()
         (when (command-queue)
           (clReleaseCommandQueue (command-queue)))
         (when (context)
           (clReleaseContext (context))))))))

(define (binary-expr rator rand1 rand2)
  (string-append "(" rand1 " " rator " " rand2 ")"))

(define idx-exprs
  (λ (strides i0 i1)
    (λ (out-i)
      (for/fold ([i0 (number->string i0)]
                 [i1 (number->string i1)]
                 [x out-i] #:result (values i0 i1))
                ([stride strides])
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
                          [stride strides]) ;; (len strides) == (len descents-out)
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

(define (ext1-ρ-kernel prim1-ρ-f)
  #<<EOF
__kernel void Kernel (__global float* v0,
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
  )

(define (run-prim1-ρ! kernel-code
                      v0 off0 size0 stride0
                      v-out size-out stride-out)
  (with-opencl
    (λ ()
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
           (clBuildProgram program (make-vector 0) (make-bytes 0))
           (set! kernel (clCreateKernel program #"Kernel"))
           (clSetKernelArg:_cl_mem kernel 0 buf0)
           (clSetKernelArg:_cl_int kernel 1 stride0)
           (clSetKernelArg:_cl_mem kernel 2 buf-out)
           (clSetKernelArg:_cl_int kernel 3 stride-out))
         (λ ()
           (set! event (clEnqueueNDRangeKernel (command-queue) kernel 1
                                               (make-vector 1 (/ size-out stride-out))
                                               (make-vector 0)
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
             (clReleaseMemObject buf0))))))))

(define functional->preallocated-1-ρ-acc
  (λ (f-acc base-shape out-shape)
    (unless (and (null? base-shape) (null? out-shape))
      (error 'ρ1-functional-non-scalar-acc
             (string-append "Functional primitives can only accept and"
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

(define (ext1-∇-kernel prim1-∇-f)
  #<<EOF
__kernel void Kernel (__global float* g0,
                      __global float* v0,
                      int stride0,
                      __global float* vz,
                      int stridez)
{

    int iz = get_global_id(0) * stridez;
    // offset is handled by the platform API
    int i0 = 0 + (iz / stridez) * stride0;

@{(prim1-∇-f "g0" "v0" "i0" "stride0"
                  "vz" "iz" "stride-z")}
}
EOF
  )

(define (run-prim1-∇! kernel-code g0
                      v0 off0 size0 stride0
                      vz offz size-z stride-z)
  (with-opencl
    (λ ()
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
           (clBuildProgram program (make-vector 0) (make-bytes 0))
           (set! kernel (clCreateKernel program #"Kernel"))
           (clSetKernelArg:_cl_mem kernel 0 buf-g)
           (clSetKernelArg:_cl_mem kernel 1 buf0)
           (clSetKernelArg:_cl_int kernel 2 stride0)
           (clSetKernelArg:_cl_mem kernel 3 buf-z)
           (clSetKernelArg:_cl_int kernel 4 stride-z))
         (λ ()
           (set! event (clEnqueueNDRangeKernel (command-queue) kernel 1
                                               (make-vector 1 (/ size-z stride-z))
                                               (make-vector 0)
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
             (clReleaseMemObject buf0))))))))

(define functional->preallocated-1-∇-acc
  (λ (f-acc base-shape out-shape)
    (unless (and (null? base-shape) (null? out-shape))
      (error '∇1-functional-non-scalar-acc
             (string-append "Functional primitives can only accept and"
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

(define (ext2-ρ-kernel prim2-ρ-f strides)
  (let*-values (((generate-idxs) (idx-exprs strides 0 0))
                ((i0-expr i1-expr) (generate-idxs "i_out")))
    #<<EOF
__kernel void Kernel (__global float* v0,
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
    ))

(define (run-prim2-ρ! kernel-code
                      v0 off0 size0 stride0
                      v1 off1 size1 stride1
                      v-out size-out stride-out)
  (with-opencl
    (λ ()
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
           (clBuildProgram program (make-vector 0) (make-bytes 0))
           (set! kernel (clCreateKernel program #"Kernel"))
           (clSetKernelArg:_cl_mem kernel 0 buf0)
           (clSetKernelArg:_cl_int kernel 1 stride0)
           (clSetKernelArg:_cl_mem kernel 2 buf1)
           (clSetKernelArg:_cl_int kernel 3 stride1)
           (clSetKernelArg:_cl_mem kernel 4 buf-out)
           (clSetKernelArg:_cl_int kernel 5 stride-out))
         (λ ()
           (set! event (clEnqueueNDRangeKernel (command-queue) kernel 1
                                               (make-vector 1 (/ size-out stride-out))
                                               (make-vector 0)
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
             (clReleaseMemObject buf0))))))))

(define functional->preallocated-2-ρ-acc
  (λ (f-acc t-shape u-shape out-shape)
    (unless (and (null? t-shape) (null? u-shape) (null? out-shape))
      (error 'ρ2-functional-non-scalar-acc
             (string-append "Functional primitives can only accept and"
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

(define (ext2-∇-kernel-atomic prim2-∇-f strides)
  (let*-values (((prim-effect0 prim-effect1) (prim2-∇-f "g"
                                                        "v0" "i0" "stride0"
                                                        "v1" "i1" "stride1"
                                                        "vz" "iz" "stride_z"))
                ((generate-idxs) (idx-exprs strides 0 0))
                ((i0-expr i1-expr) (generate-idxs "iz")))
    #<<EOF
__kernel void Kernel (__global float* g0,
                      __global float* g1,
                      __global float* v0,
                      int stride0,
                      __global float* v1,
                      int stride1,
                      __global float* vz,
                      int stride_z)
{

    int iz = get_global_id(0) * stride_z;
    int i0 = @{i0-expr};
    int i1 = @{i1-expr};
    __global float *g;

    g = g0;
@{prim-effect0}

    g = g1;
@{prim-effect1}
}
EOF
    ))

(define (ext2-∇-kernel-split prim2-∇-f strides
                             s0 s1 r0 r1 s-out r-out)
  (let*-values (((prim-effect0 prim-effect1) (prim2-∇-f "g"
                                                        "v0" "i0" "stride0"
                                                        "v1" "i1" "stride1"
                                                        "vz" "iz" "stride_z"))
                ((repeats0 repeats1) (calc-repeats s0 s1 r0 r1 s-out r-out))
                ((generate-idxs) (idx-exprs strides 0 0))
                ((generate-idxs-inv) (idx-exprs-inv strides 0 repeats0 repeats1 s-out))
                ((i0-expr i1-expr) (generate-idxs "iz"))
                ((iz-expr0 iz-expr1) (generate-idxs-inv "i0" "i1" "i_rep")))
    (values
     #<<EOF
__kernel void Kernel (__global float* g,
                      __global float* v0,
                      int stride0,
                      __global float* v1,
                      int stride1,
                      __global float* vz,
                      int stride_z)
{
    int i0 = get_global_id(0) * stride0;

    for(int i_rep=0; i_rep<@{repeats0}; i_rep++) {
        int iz = @{iz-expr0};
        int i1 = @{i1-expr};

@{prim-effect0}
    }
}
EOF

     #<<EOF
__kernel void Kernel (__global float* g,
                      __global float* v0,
                      int stride0,
                      __global float* v1,
                      int stride1,
                      __global float* vz,
                      int stride_z)
{
    int i1 = get_global_id(0) * stride1;

    for(int i_rep=0; i_rep<@{repeats1}; i_rep++) {
        int iz = @{iz-expr1};
        int i0 = @{i0-expr};

@{prim-effect1}
    }
}
EOF
     )))

(define (run-prim2-∇-atomic! kernel-code g0 g1
                             v0 off0 size0 stride0
                             v1 off1 size1 stride1
                             vz offz size-z stride-z)
  (with-opencl
    (λ ()
      (let* ([buf0 #f]
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
           (clBuildProgram program (make-vector 0) (make-bytes 0))
           (set! kernel (clCreateKernel program #"Kernel"))
           (clSetKernelArg:_cl_mem kernel 0 buf-g0)
           (clSetKernelArg:_cl_mem kernel 1 buf-g1)
           (clSetKernelArg:_cl_mem kernel 2 buf0)
           (clSetKernelArg:_cl_int kernel 3 stride0)
           (clSetKernelArg:_cl_mem kernel 4 buf1)
           (clSetKernelArg:_cl_int kernel 5 stride1)
           (clSetKernelArg:_cl_mem kernel 6 buf-z)
           (clSetKernelArg:_cl_int kernel 7 stride-z))
         (λ ()
           (set! event (clEnqueueNDRangeKernel (command-queue) kernel 1
                                               (make-vector 1 (/ size-z stride-z))
                                               (make-vector 0)
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
             (clReleaseMemObject buf0))))))))

(define (run-prim2-∇-split! kernel-code0 kernel-code1 g0 g1
                            v0 off0 size0 stride0
                            v1 off1 size1 stride1
                            vz offz size-z stride-z)
  (with-opencl
      (λ ()
        (define (run! kernel-code g size-in stride-in)
          (let* ([buf0 #f]
                 [buf1 #f]
                 [buf-z #f]
                 [buf-g #f]
                 [program #f]
                 [kernel #f]
                 [event #f])
            (dynamic-wind
             (λ ()
               ;; Exclude memory consumed by elements before offset of input vector v0
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
               (set! buf-g (clCreateBuffer (context) 'CL_MEM_WRITE_ONLY
                                           (* (ctype-sizeof _cl_float)
                                              size-in)
                                           #f))
               (set! program (clCreateProgramWithSource
                              (context)
                              (make-vector 1 (string->bytes/utf-8 kernel-code))))
               (clBuildProgram program (make-vector 0) (make-bytes 0))
               (set! kernel (clCreateKernel program #"Kernel"))
               (clSetKernelArg:_cl_mem kernel 0 buf-g)
               (clSetKernelArg:_cl_mem kernel 1 buf0)
               (clSetKernelArg:_cl_int kernel 2 stride0)
               (clSetKernelArg:_cl_mem kernel 3 buf1)
               (clSetKernelArg:_cl_int kernel 4 stride1)
               (clSetKernelArg:_cl_mem kernel 5 buf-z)
               (clSetKernelArg:_cl_int kernel 6 stride-z))
             (λ ()
               (set! event (clEnqueueNDRangeKernel (command-queue) kernel 1
                                                   (make-vector 1 (/ size-in stride-in))
                                                   (make-vector 0)
                                                   (make-vector 0)))
               (set! event (clEnqueueReadBuffer (command-queue) buf-g 'CL_TRUE 0
                                                (* (ctype-sizeof _cl_float)
                                                   size-in)
                                                (vec->cpointer g) (vector event))))
             (λ ()
               (when kernel
                 (clReleaseKernel kernel))
               (when program
                 (clReleaseProgram program))
               (when buf-g
                 (clReleaseMemObject buf-g))
               (when buf-z
                 (clReleaseMemObject buf-z))
               (when buf1
                 (clReleaseMemObject buf1))
               (when buf0
                 (clReleaseMemObject buf0))))))
        (run! kernel-code0 g0 size0 stride0)
        (run! kernel-code1 g1 size1 stride1))))

(define functional->preallocated-2-∇-acc
  (λ (f-acc t-shape u-shape out-shape)
    (unless (and (null? t-shape) (null? u-shape) (null? out-shape))
      (error '∇2-functional-non-scalar-acc
             (string-append "Functional primitives can only accept and"
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

(provide run-prim1-ρ! functional->preallocated-1-ρ-acc ext1-ρ-kernel
         run-prim1-∇! functional->preallocated-1-∇-acc ext1-∇-kernel
         run-prim2-ρ! functional->preallocated-2-ρ-acc ext2-ρ-kernel
         run-prim2-∇-atomic! run-prim2-∇-split!
         functional->preallocated-2-∇-acc
         ext2-∇-kernel-atomic ext2-∇-kernel-split)