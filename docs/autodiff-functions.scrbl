#lang scribble/manual
@title{Automatic Differentiation}

@defmodule[malt]

[TODO: These type definitions need to be cleaned up]
For the following, we define the types
@itemlist[
@item{@racket[dual?] - an object made up of a @racket[tensor?] (called its real part) and a @racket[link?] (called its link)}
@item{@racket[gradient-state?] - a hashtable from a @racket[dual?] to a @racket[tensor?]}
@item{@racket[link?] - a function of the type
@codeblock{(-> (dual? tensor? gradient-state?) gradient-state?)}
}
@item{@racket[differentiable?] - Either a @racket[dual?], or a @racket[(listof differentiable?)]}
]

@defproc[(dual [ρ tensor?] [κ link?]) dual?]{
Constructs a dual with ρ as its real part, and κ as its link.
}

@defproc[(dual? [x any]) boolean?]{
Returns @racket[#t] if @racket[x] is a dual.
}

@defproc[(ρ [d (or tensor? dual?)]) tensor?]{
If @racket[d] is a @racket[tensor?], returns @racket[d]. Otherwise, returns the real part of the dual.
}

@defproc[(κ [d (or tensor? dual?)]) link?]{
If @racket[d] is a @racket[tensor?], returns the function @racket[end-of-chain]. Otherwise, returns the link of the dual.
}

@defproc[(scalar? [x any]) boolean?]{
Returns @racket[#t] if @racket[x] is a @racket[number?] or @racket[x] is a @racket[dual?] and @racket[(ρ x)] is a @racket[number?]
}

@defproc[(end-of-chain [d dual?] [z tensor?] [σ gradient-state?]) gradient-state]{
The default link that terminates gradient computation for any dual. It returns a
new @racket[gradient-state?] that includes the mapping of @racket[d] to the addition  of @racket[z]
and the mapping of @racket[d] in @racket[σ], if it exists, or 0.0 otherwise. If @racket[z] is not a @racket[number?],
@racket[+-ρ] is used for the addition.
}

@defproc[(∇¹ [f (-> (differentiable? ...) differentiable?)] [t0 differentiable?] ... [tn differentiable?]) (listof tensor?)]{
Returns a list of gradients @racket[(list g0 ... gn)] where @racket[gi] is the gradient of @racket[(f t0 ... tn)] with
respect to @racket[ti]. If @racket[(f t0 ... tn)] is not a @racket[scalar?], then @racket[gi] is the sum of the gradients
of each scalar in @racket[(f t0 ... tn)] with respect to @racket[ti].
}

@defproc[(∇ [f (-> ((listof tensor?)) tensor?)] [θ (listof tensor?)]) (listof tensor?)]{
This is equivalent to @codeblock{
(ref (∇¹ f θ) 0)
}
}

@defproc[(gradient-of [f (-> ((listof tensor?)) tensor?)] [θ (listof tensor?)]) (listof tensor?)]{
This is also equivalent to @codeblock{
(ref (∇¹ f θ) 0)
}
}
