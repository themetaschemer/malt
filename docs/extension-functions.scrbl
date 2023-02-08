#lang scribble/manual
@title{Extended Functions}

@defmodule*[(malt malt/base malt/base-no-duals malt/base-no-overrides malt/base-no-duals-no-overrides malt/learner malt/flat-tensors malt/nested-tensors)]

Base functions are commonly understood functions that only work on tensors of a specific rank known as the @italic{base rank}.
Base functions can be extended to work with tensors of any rank higher than the base rank.

Additionally, all the base and extended functions are differentiable
(as described in @secref{overview}). Here we define a
@italic{differentiable function} as a Racket function that can be used
with the automatic differentiation functions provided by @racket[malt]
to produce valid gradients.

The following types, as described in @secref{overview} are assumed
@itemlist[
@item{@racket[primitive-1?] A differentiable function of one argument that is constructed from the invocation of @racket[prim1].}
@item{@racket[primitive-2?] A differentiable function of two arguments that is constructed from the invocation of @racket[prim2].}
]


@defproc[(ext1 [prim primitive-1?] [base-rank natural?]) primitive-1?] {
  Extends @racket[prim] to operate on tensors of rank higher than @racket[base-rank]. The returned function is also a @racket[primitive-1?].
}

@defproc[(ext2 [prim primitive-2?] [base-rank-1 natural?] [base-rank-2 natural?]) primitive-2?]{
  Extends @racket[prim] to operate on tensors of rank higher than @racket[base-rank-1] in the first argument and higher than @racket[base-rank-2] in the second argument. The returned function is also a @racket[primitive-2?].
}

@section{Primitives in @racket[learner] and @racket[nested-tensors]}
@defmodule*[(malt malt/base malt/base-no-duals malt/base-no-duals-no-overrides malt/learner malt/nested-tensors)]
The following functions are available when the default tensor implementation in @racket[malt] is @racket[learner] or @racket[nested-tensors].

@defproc[(prim1 [ρ-fn (-> (tensor?) tensor?)] [∇-fn (-> (tensor? tensor?) tensor?)]) primitive-1?]{
  Constructs a differentiable function (known as a @italic{primitive}) of one tensor argument that invokes @racket[ρ-fn] to compute the result of
  the application of the primitive, and uses @racket[∇-fn] to find the gradient of the result with respect
  the argument provided to the primitive.
}

@defproc[(prim2 [ρ-fn (-> (tensor? tensor?) tensor?)] [∇-fn (-> (tensor? tensor? tensor?) tensor?)]) primitive-2?]{
  Constructs a differentiable function (known as a @italic{primitive}) of two tensor arguments that invokes @racket[ρ-fn] to compute the result of
  the application of the primitive, and uses @racket[∇-fn] to find the gradient of the result with respect
  the two arguments provided to the primitive.
}


@section{Primitives in @racket[flat-tensors]}

@defmodule*[(malt malt/base malt/base-no-duals malt/base-no-overrides malt/flat-tensors)]
The following functions are available when the default tensor implementation in @racket[malt] is @racket[flat-tensors].

@defproc[(prim1 [ρ-fn (-> (tensor?) tensor?)] [∇-fn (-> (tensor? tensor?) tensor?)] [shape-fn (-> shape? shape?)]) primitive-1?]{
  Constructs a differentiable function (known as a @italic{primitive}) of one tensor argument that invokes @racket[ρ-fn] to compute the result of
  the application of the primitive, and uses @racket[∇-fn] to find the gradient of the result with respect
  the argument provided to the primitive. The third argument, @racket[shape-fn], when invoked with the shape corresponding
  to the shape of the argument to the primitive, provides the shape of the result from the invocation of @racket[ρ-fn] on that argument.
}

@defproc[(prim2 [ρ-fn (-> (tensor? tensor?) tensor?)] [∇-fn (-> (tensor? tensor? tensor?) tensor?)] [shape-fn (-> (shape? shape?) shape?)]) primitive-2?]{
  Constructs a differentiable function (known as a @italic{primitive}) of two tensor arguments that invokes @racket[ρ-fn] to compute the result of
  the application of the primitive, and uses @racket[∇-fn] to find the gradient of the result with respect
  the arguments provided to the primitive. The third argument, @racket[shape-fn], when invoked with the shapes corresponding
  to the arguments to the primitive, provides the shape of the result from the invocation of @racket[ρ-fn] on those two arguments.
}

@section{Extension in @racket[flat-tensors] and @racket[nested-tensors]}

@defmodule*[(malt malt/base malt/base-no-duals malt/base-no-overrides malt/flat-tensors malt/nested-tensors)]

The following functions are available when the default tensor implementation in @racket[malt] is @racket[flat-tensors] or @racket[nested-tensors].
These can be used to provide new tensor operations that cannot
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
