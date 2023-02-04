#lang scribble/manual
@title[#:tag "overview"]{Overview}

The toolkit has four major pieces that are important to understand before
reading through this documentation. These four pieces are
@itemlist[
@item{Tensors}
@item{Automatic Differentiation}
@item{Operator Extension}
@item{Deep Learning Functions}
]

These pieces work together to form the whole toolkit. This overview
provides a high level roadmap in how to understand this documentation
and the accompanying code.

@section[#:tag "overview-tensors"]{Tensors}

A @italic{tensor} is the fundamental data structure in deep learning. A tensor
can be thought of an @italic{n}-dimensional array where it is possible for @italic{n}
to be 0. When @italic{n} is 0, the tensor is known as a @italic{scalar}.

The easiest way to think about it is that a scalar is a single number and tensors
are vectors of scalars or vectors of tensors.

Every tensor has a @italic{shape}. The shape of a tensor is a list of @italic{n}
members where the @italic{i}th member of the list is the size of the @italic{i}th dimension
of the tensor. For scalars, the shape is the empty list.

The number @italic{n} is known as the rank of the tensor.
The following types are used to denote entities related to tensors.
@itemlist[
@item{@racket[scalar?] - Tensors of rank 0}
@item{@racket[tensor?] - Tensors of rank 0 or higher}
@item{@racket[shape?] - The type @racket[(listof natural?)] signifies the shape of a tensor.}
]



@section[#:tag "overview-ad"]{Automatic Differentiation}

Malt provides a simple reverse-mode automatic differentiation mechanism that is based
on the concept of @italic{duals}. A dual carries the tensor result of a function along
with a @italic{link} which encodes the chain of operations that produced the tensor result.
This allows the gradient of the function that produced the tensor result to be computed.

Duals are automatically constructed when differentiable primitive operators (also provided
by Malt) are used.

For interoperability, numerical constants are also considered to be duals with an empty link
(known as @racket[end-of-chain]).

Duals and tensors can contain each other, depending upon the implementation. Malt provides
three implementations of tensors in increasing order of complexity and efficiency.

@itemlist[
@item{@racket[learner] - This implementation is the simplest. Tensors are implemented as nested vectors and all scalars are duals.
This is the implementation that follows the pedagogy of @italic{The Little Learner}.
}
@item{@racket[nested-tensors] - This implementation is a little more involved. Both tensors and scalars are duals. Tensors
are implemented as nested vectors, but they may not contain duals. Unlike in the @racket[learner] implementation,
here the links are associated directly with the tensor as opposed to each scalar in the tensor. Because of this,
automatic differentiation is more efficient when compared to @racket[learner].
}
@item{@racket[flat-tensors] - This implementation is the most efficient among the three. Both tensors and scalars are duals. Tensors
are implemented as flat vectors (similar to how arrays are implemented in C or Fortran), but they also may not contain duals.
Here as well, the links are associated directly with the tensor as opposed to each scalar in the tensor. The flat organization
of the tensors makes this the most efficient implementation among the three.
}
]

The default implementation for tensors in Malt is @racket[flat-tensors]. When developing with @racket[malt], implementations can be changed
by recompiling the code with the appropriate choice of implementation.

The following types are used to denote entities related to duals.
@itemlist[
@item{@racket[dual?] - Duals}
@item{@racket[link?] - Links included in a dual. Defined as the type
@codeblock{(-> (dual? tensor? gradient-state?) gradient-state?)}}
@item{@racket[gradient-state?] - A hashtable from @racket[dual?] to @racket[tensor?]}
@item{@racket[differentiable?] - Either a @racket[dual?], or a @racket[(listof differentiable?)]. In the @racket[learner]
implementation @racket[(vectorof differentiable?)] is also considered to be @racket[differentiable?], but not in other
implementations.}
]

@section[#:tag "overview-ext"]{Operator Extension}

The simple recursive structure of tensors allows
commonly used numerical primitives to be extended to produce
what are known as @italic{pointwise extensions}. These are also
known as @italic{broadcast} operations over arrays. Malt provides
an additional ability to pause the extension at a certain rank.
So, rather than go all the way to the scalars in the array, the
extension can stop at one of the higher dimensions. This allows
the construction of polynomial-complexity functions
by composing extensions.

Additionally, these extended primitives are automatically differentiable
and functions built by composing these primitives can also
be automatically differentiated (within limits of differentiablity of
the function).

Section @secref{diffable-fns} lists the primitives provided by Malt. Malt
also provides tools to build extended versions of user defined functions.
The type signatures of these tools are specific to the implementation of
tensors described above.

The following types are used to denote entities related to operator extension.
@itemlist[
@item{@racket[primitive-1?] - A unary non-extended primitive.}
@item{@racket[primitive-2?] - A binary non-extended primitive.}
]

@section[#:tag "overview-deep"]{Deep Learning Functions}

Building on top of tensors and automatic differentiation, Malt provides
a collection of deep learning specific functions -- loss functions, layer
functions, gradient descent, compositional mechanisms, hyperparameters, etc.

The following types are used to describe some of these functions.
@itemlist[
@item{@racket[theta?] - A list of tensors which forms a parameter set.}
]

@section[#:tag "overview-types"]{Summary of Types}

The following types are used primarily in the description of functions,
but some types are marked as "virtual", in the sense that
predicates for the type are not defined, but their intent is clear.

@itemlist[
@item{@racket[scalar?] - Tensors of rank 0}
@item{@racket[tensor?] - Tensors of rank 0 or higher}
@item{@racket[shape?] - The type @racket[(listof natural?)] signifies the shape of a tensor. (virtual)}
@item{@racket[dual?] - Duals}
@item{@racket[link?] - Links included in a dual. Defined as the type
@codeblock{(-> (dual? tensor? gradient-state?) gradient-state?)}}
@item{@racket[gradient-state?] - A hashtable from @racket[dual?] to @racket[tensor?]}
@item{@racket[differentiable?] - Either a @racket[dual?], or a @racket[(listof differentiable?)]. In the @racket[learner]
implementation @racket[(vectorof differentiable?)] is also considered to be @racket[differentiable?], but not in other
implementations.}
@item{@racket[primitive-1?] - A unary non-extended primitive. (virtual)}
@item{@racket[primitive-2?] - A binary non-extended primitive. (virtual)}
@item{@racket[theta?] - A list of tensors which forms a parameter set. (virtual)}
]