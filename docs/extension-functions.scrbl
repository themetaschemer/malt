#lang scribble/manual
@title{Operator Extension}

@defmodule[malt]


Here we define a @italic{differentiable function} as a Racket function
that can be used with the automatic differentiation functions provided
by @racket[malt] to produce valid gradients.

The following types are assumed
@itemlist[
@item{@racket[primitive-1?] A differentiable function of one argument that is constructed from the invocation of @racket[prim1]}
@item{@racket[primitive-2?] A differentiable function of two arguments that is constructed from the invocation of @racket[prim2]}
]

@defproc[(prim1 [ρ-fn (-> (tensor?) tensor?)] [∇-fn (-> (tensor? tensor?) tensor?)]) primitive-1?]{
  For @racket[learner] and @racket[nested-tensors] implementations.

  Constructs a differentiable function (known as a @italic{primitive}) of one tensor argument that invokes @racket[ρ-fn] to compute the result of
  the application of the primitive, and uses @racket[∇-fn] to find the gradient of the result with respect
  the argument provided to the primitive.
}

@defproc[(prim1 [ρ-fn (-> (tensor?) tensor?)] [∇-fn (-> (tensor? tensor?) tensor?)] [shape-fn (-> (listof natural?) (listof natural?))]) primitive-1?]{
  For  @racket[flat-tensors] implementations.

  Constructs a differentiable function (known as a @italic{primitive}) of one tensor argument that invokes @racket[ρ-fn] to compute the result of
  the application of the primitive, and uses @racket[∇-fn] to find the gradient of the result with respect
  the argument provided to the primitive. The third argument, @racket[shape-fn], when invoked with the shape corresponding
  to the shape of the argument to the primitive, provides the shape of the result from the invocation of @racket[ρ-fn] on that argument.
}

@defproc[(prim2 [ρ-fn (-> (tensor? tensor?) tensor?)] [∇-fn (-> (tensor? tensor? tensor?) tensor?)]) primitive-2?]{
  Constructs a differentiable function (known as a @italic{primitive}) of two tensor arguments that invokes @racket[ρ-fn] to compute the result of
  the application of the primitive, and uses @racket[∇-fn] to find the gradient of the result with respect
  the two arguments provided to the primitive.
}

@defproc[(prim2 [ρ-fn (-> (tensor? tensor?) tensor?)] [∇-fn (-> (tensor? tensor? tensor?) tensor?)] [shape-fn (-> ((listof natural?) (listof natural?)) (listof natural?))]) primitive-2?]{
  For  @racket[flat-tensors] implementations.

  Constructs a differentiable function (known as a @italic{primitive}) of two tensor arguments that invokes @racket[ρ-fn] to compute the result of
  the application of the primitive, and uses @racket[∇-fn] to find the gradient of the result with respect
  the arguments provided to the primitive. The third argument, @racket[shape-fn], when invoked with the shapes corresponding
  to the arguments to the primitive, provides the shape of the result from the invocation of @racket[ρ-fn] on those two arguments.
}


@defproc[(ext1 [prim primitive-1?] [base-rank natural?]) primitive-1?] {
  Extends @racket[prim] to operate on tensors of rank higher than @racket[base-rank]. The returned function is also a @racket[primitive-1?]
}

@defproc[(ext2 [prim primitive-2?] [base-rank-1 natural?] [base-rank-2 natural?]) primitive-2?] {
  Extends @racket[prim] to operate on tensors of rank higher than @racket[base-rank-1] in the first argument and higher than @racket[base-rank-2] in the second argument. The returned function is also a @racket[primitive-2?]
}

The following functions are implementation dependent and are used to provide new tensor operations that cannot
be constructed using @racket[ext1] and @racket[ext2]. The following is a guideline, but the actual type of
@racket[ρ-fn] and @racket[∇-fn] arguments will vary by implementation. See the @racket[malt] source code
for further examples.

@defproc[(ext1-ρ [ρ-fn (-> (tensor?) tensor?)] [base-rank natural?]) (-> (tensor?) tensor?)]{
  Extends @racket[ρ-fn] to operate on tensors of rank higher than @racket[base-rank]. The returned function is not differentiable.
  @racket[ext1-ρ] can be used to construct functions that can be the first argument to @racket[prim1].
}

@defproc[(ext1-∇ [∇-fn (-> (tensor? tensor?) tensor?)] [base-rank natural?]) (-> (tensor? tensor?) tensor?)]{
  Extends @racket[∇-fn] to operate on tensors of rank higher than @racket[base-rank]. The returned function is not differentiable.
  @racket[ext1-∇] can be used to construct functions that can be the second argument to @racket[prim1].
}

@defproc[(ext2-ρ [ρ-fn (-> (tensor? tensor?) tensor?)] [base-rank-1 natural?] [base-rank-2 natural?]) (-> (tensor? tensor?) tensor?)]{
  Extends @racket[ρ-fn] to operate on tensors of rank higher than @racket[base-rank-1] in the first argument and higher than @racket[base-rank-2] in the second argument.
  The returned function is not differentiable.  @racket[ext2-ρ] can be used to construct functions that can be the first argument to @racket[prim2].
}

@defproc[(ext2-∇ [∇-fn (-> (tensor? tensor?) tensor?)] [base-rank natural?]) (-> (tensor? tensor?) tensor?)]{
  Extends @racket[∇-fn] to operate on tensors of rank higher than @racket[base-rank-1] in the first argument and higher than @racket[base-rank-2] in the second argument. The returned function is not differentiable.
  @racket[ext2-∇] can be used to construct functions that can be the second argument to @racket[prim2].
}
