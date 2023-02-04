#lang scribble/manual
@title{Entry points}

Malt provides facilities to explore and develop different pieces (as described in the @secref{overview}) of the deep
learning stack, independently of what is provided in the package itself.

This section provides a guide for the various modules that can be @code{require}'d
for different purposes. We refer to these @code{require} invocations as @italic{entry points} into @racket[malt]

@defmodule[malt]
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

@defmodule[malt/base]
This entry point provides everything in @racket[malt], but leaves out deep learning specific
function exports.

This entry point can be used by readers and other developers to write
machine learning specific code using the provided tensor
implementations, either as they follow along the chapters in the book,
or develop their own new functions for deep learning.

@defmodule[malt/base-no-duals]
This entry point provides every thing in @racket[malt/base] but leaves out
automatic differentiation and deep learning specific function exports.

This entry point can be used by readers and other developers to write
automatic differentiation and use that to build machine learning specific code, either as
they follow along the chapters in the book, or develop their own new
functions for these purposes.

@defmodule[malt/base-no-overrides]

The entry points @code{malt}, @code{malt/base}, and  @code{malt/base-no-duals} by default override common
mathematical functions (@racket[+], @racket[*] etc. See @secref{diffable-fns} and @secref{non-diffable-fns}).

This entry point does not override any default mathematical operations. The differentiable operations are
provided with a @code{d} or a @code{d-} prefix. Section @secref{diffable-fns} lists the names that are used
for these operations.

This entry point is useful in the cases when overriding default mathematical operations is overly restrictive.

@defmodule[malt/base-no-overrides-no-duals]

This entry point does not override any default mathematical operations, nor does it provide automatic differentiation.
See @secref{non-diffable-fns}.

@defmodule[malt/learner]

This entry point provides the tensor implementation, automatic differentiation, and extended operators to allow users
to develop code exactly as it appears in @italic{The Little Learner}. It excludes all the deep learning specific operations.


@defmodule[malt/nested-tensors]

This entry point provides the tensor implementation, automatic differentiation, and extended operators for the nested
tensor representation referred to in @italic{Appendix B. I Could Have Raced All Day}. It can be used for experimentation
using this representation.

@defmodule[malt/flat-tensors]

This entry point provides the tensor implementation, automatic differentiation, and extended operators for the flat
tensor representation referred to in @italic{Appendix B. I Could Have Raced All Day}. This is the default implementation
of tensors when using the @code{malt} entry point.

@defmodule[malt/tools]

When using restricted entry points @code{malt/learner},
@code{malt/nested-tensors} or @code{malt/flat-tensors}, this entry
point can be used to provide hyperparameters, normally distributed
random numbers, and logging tools.
