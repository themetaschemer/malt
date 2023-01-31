#lang scribble/manual
@title{Entry points}
Malt is a package that provides multiple entry points in order to
explore and develop different parts of the deep learning stack, independently
of what is provided in the package itself.

This section provides a guide for the various modules that can be @code{require}'d
for different purposes. We refer to these @code{require} invocations as @italic{entry points}.

@defmodule[malt]
This entry point covers the entire set of modules provide the @code{malt} package. It provides
all functions related to lists, tensors, automatic differentiation, extended operators,
hyperparameters, random number generation, logging, loss functions, layer functions, gradient descent,
dense layers, 1-D convolution layers and He initialization.

It can be used to develop example neural networks, such the @italic{iris} classifier from
@italic{The Little Learner}
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
This entry point only provides
all functions related to lists, tensors, automatic differentiation, extended operators,
hyperparameters, random number generation, and logging. It leaves out deep learning specific
function exports.

This entry point can be used by students and other developers to
write machine learning specific code, either as they follow along the
chapters in the book, or develop their own new functions for the purpose.

@defmodule[malt/base-no-duals]
This entry point only provides all
functions related to lists, tensors, extended operators,
hyperparameters, random number generation, and logging. It leaves out
automatic differentiation and deep learning specific function exports.

This entry point can be used by students and other developers to write
automatic differentiation or machine learning specific code, either as
they follow along the chapters in the book, or develop their own new
functions for these purposes.

@defmodule[malt/base-no-overrides]

The entry points @code{malt}, @code{malt/base} and  @code{malt/base-no-duals} by default override common
mathematical functions (see @secref{overrides} for the list of functions overriden below).

This entry point does not override any default mathematical operations. The differentiable operations are
provided with a @code{d} or a @code{d-} prefix. Section @secref{overrides} lists the names that are used
for these operations.

This entry point is useful when overriding default mathematical operations is overly restrictive.

@defmodule[malt/base-no-overrides-no-duals]

This entry point does not override any default mathematical operations, nor does it provide automatic differentiation.

@defmodule[malt/learner]

This entry point provides the tensor implementation, automatic differentiation and extended operators to allow users
to develop code exactly as it appears in @italic{The Little Learner}.

@defmodule[malt/nested-tensors]

This entry point provides the tensor implementation, automatic differentiation and extended operators for the nested
tensor representation referred to in @italic{Appendix B. I Could Have Raced All Day}. It can be used for experimentation
using this representation.

@defmodule[malt/flat-tensors]

This entry point provides the tensor implementation, automatic differentiation and extended operators for the flat
tensor representation referred to in @italic{Appendix B. I Could Have Raced All Day}. This is the default implementation
of tensors when using the @code{malt} entry point.

@defmodule[malt/tools]

When using restricted entry points @code{malt/learner},
@code{malt/nested-tensors} or @code{malt/flat-tensors}, this entry
point can be used to provide hyperparameters, normally distributed
random numbers and logging tools.

@section[#:tag "overrides"]{Overriden operators and their distinct
names}
The following mathematical functions are overriden by @code{malt}, @code{malt/base}
@code{malt/learner}, @code{malt/nested-tensors}, and @code{malt/flat-tensors}.

@code{+ - * /} - These mathematical operations are overriden by the functions @code{d+}, @code{d-}, @code{d*}, and @code{d/}
The prefix @code{d} denotes the ability to differentiate the results. All these operators are extended to work with
the default implementation of tensors.

@code{exp log expt sqrt sqr} - These mathematical operations are
overriden by the functions @code{d-exp}, @code{d-log},
@code{d-expt}, @code{d-sqrt}, and @code{d-sqr}. The prefix @code{d-} denotes
the ability to differentiate these functions. All these operators are
extended to work with the default implementation of tensors.

@code{sum max abs argmax} - These mathematical operations are
overriden by the functions @code{d-sum}, @code{d-max}, @code{d-abs}, and @code{d-argmax}.
All these operators are extended to work with the default implementation of tensors of rank 1 or higher.

The following mathematical functions are overriden by @code{malt/no-duals}.

@code{+ - * / exp log expt sqrt sqr sum max abs argmax} - These mathematical operations are overriden by the functions of the same
name but with an additional suffix @code{-ρ}. For example, @code{+-ρ}, @code{--ρ}, @code{*-ρ}, and @code{/-ρ}.
The suffix  denotes that these are operators that are extended to work with
the default implementation of tensors, but are not supported by automatic differentation.
