#lang racket
(require (for-syntax "impl-loader.rkt"))
(require rackunit)
(require opencl/c)
(require ffi/unsafe)
(require ffi/unsafe/define)

(provide unsafe-test (rename-out (clBuildProgram clBuildProgram^)))

;; This macro will disable tests which give weird errors on certain hardware
;; runtimes. In the future, we will use this macro to switch testing behaviour
;; so tests run without failing due to hardware issues.
(define-syntax (unsafe-test x)
  (syntax-case x ()
    [(_ test) (disable-unsafe-tests?) #'(check-true #t)]
    [(_ test) #'test]))

;; TODO: Make custom clBuildProgram FFI binding which accepts the pfn_notify function pointer param
(define-ffi-definer define-opencl (ffi-lib "libOpenCL"))

(define-opencl clBuildProgram
  (_fun [program : _cl_program]
        [num_devices : _cl_uint = (vector-length device_list)]
        [device_list : (_vector i _cl_device_id)]
        [options : _bytes]
        [pfn_notify : (_fun _cl_program _void* -> _void)]
        [user_data : _void*]
        -> [status : _cl_int]
        ->
        (cond [(= status CL_SUCCESS) (void)]
              [(= status CL_INVALID_PROGRAM)
               (error 'clBuildProgram "program is not a valid program object")]
              [(= status CL_INVALID_VALUE)
               (error 'clBuildProgram "device_list is NULL and num_devices is greater than zero or device_list is not NULL and num_devices is zero or pfn_notify is NULL but user_data is not NULL")]
              [(= status CL_INVALID_DEVICE)
               (error 'clBuildProgram "OpenCL devices listed in device_list are not in the list of devices associated with program")]
              [(= status CL_INVALID_BINARY)
               (error 'clBuildProgram "program is created with clCreateWithProgramBinary and devices listed in device_list do not have a valid program binary loaded.")]
              [(= status CL_INVALID_BUILD_OPTIONS)
               (error 'clBuildProgram "the build options specified by options are invalid")]
              [(= status CL_INVALID_OPERATION)
               (error 'clBuildProgram "the build of a program for any of the devies listed in device_list by a previous call to clBuildProgram for program has not completed")]
              [(= status CL_COMPILER_NOT_AVAILABLE)
               (error 'clBuildProgram "program is created with clCreateProgramWithSource and a compiler is not available")]
              [(= status CL_BUILD_PROGRAM_FAILURE)
               (error 'clBuildProgram "there is a failure to build the program executable")]
              [(= status CL_INVALID_OPERATION)
               (error 'clBuildProgram "there are kernel objects attached to program")]
              [(= status CL_OUT_OF_HOST_MEMORY)
               (error 'clBuildProgram "there is a failure to allocate resources required by the OpenCL implementation on the host")]
              [else
               (error 'clBuildProgram "Invalid error code: ~e" status)])))
