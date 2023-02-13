#lang scribble/manual
@title{Entry points}

Malt provides facilities to explore and develop different pieces (as described in the @secref{overview}) of the deep
learning stack, independently of what is provided in the package itself.

This section provides a guide for the various modules that can be @code{require}'d
for different purposes. We refer to these @code{require} invocations as @italic{entry points} into @racket[malt]

@defmodule[malt #:no-declare]
This entry point covers the entire set of modules provided by the @code{malt} package. It provides
all functions related to lists, tensors, automatic differentiation, extended operators,
hyperparameters, random number generation, logging, loss functions, layer functions, gradient descent,
dense layers, 1-D convolution layers and He initialization.

It can be used to develop example neural networks, such as the @italic{iris} classifier, a dense network with
a single hidden layer from @italic{The Little Learner}.
@codeblock{
(require malt)

(define dense-block
  (λ (n m)
    (block relu
      (list
        (list m n)
        (list m)))))

(define iris-network
  (stack-blocks
    (list
      (dense-block 4 8)
      (dense-block 8 3))))
}

@defmodule[malt/base #:no-declare]
This entry point provides everything in @racket[malt], but leaves out deep learning specific
functions.

This entry point is used by other sub-modules within Malt where only tensors and automatic
differentiation are required.

@defmodule[malt/base-no-duals #:no-declare]
This entry point provides everything in @racket[malt/base] but leaves out
automatic differentiation and deep learning specific function exports.

This entry point is used by some sub-modules within Malt where tensors and extended operations
are required, but duals and automatic differentiation are not.


@defmodule[malt/base-no-overrides #:no-declare]

The entry points @code{malt}, @code{malt/base}, and  @code{malt/base-no-duals} by default override common
mathematical functions (@racket[+], @racket[*], etc. See @secref{diffable-fns} and @secref{non-diffable-fns}).

This entry point does not override any default mathematical operations. The differentiable operations are
provided with a @code{d} or a @code{d-} prefix. Section @secref{diffable-fns} lists the names that are used
for these operations.

This entry point is used by some sub-modules within Malt where both differentiable extended operations as well as Racket's
primitive operations are necessary.

@defmodule[malt/base-no-overrides-no-duals #:no-declare]

This entry point does not override any default mathematical operations, nor does it provide automatic differentiation.
See @secref{non-diffable-fns}.

This entry point is used by some sub-modules within Malt where both non-differentiable extended operations as well as Racket's
primitive operations are necessary.


@defmodule[malt/learner #:no-declare]

This entry point provides the tensor representation, automatic differentiation, and extended operators to allow users
to develop code exactly as it appears in @italic{The Little Learner}. It excludes all the deep learning specific operations.


@defmodule[malt/nested-tensors #:no-declare]

This entry point provides the tensor representation, automatic differentiation, and extended operators for the nested
tensor representation referred to in @italic{Appendix B. I Could Have Raced All Day}. It can be used for experimentation
using this representation.

@defmodule[malt/flat-tensors #:no-declare]

This entry point provides the tensor representation, automatic differentiation, and extended operators for the flat
tensor representation referred to in @italic{Appendix B. I Could Have Raced All Day}. This is the default representation
of tensors when using the @code{malt} entry point.

@defmodule[malt/tools #:no-declare]

When using entry points @code{malt/learner},
@code{malt/nested-tensors} or @code{malt/flat-tensors}, this entry
point can be used to provide hyperparameters, normally distributed
random numbers, and logging tools. In order to use @racket[malt/tools]
with a given representation, they must be recompiled for that specific representation.
Follow the instructions provided in the @hyperlink["https://github.com/themetaschemer/malt"]{repository}.
