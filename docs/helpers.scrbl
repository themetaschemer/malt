#lang scribble/manual
@title{Utilities}

@defmodule*[(malt malt/base malt/base-no-overrides malt/learner malt/flat-tensors malt/nested-tensors)]

These are utilities to help with debugging and testing functions written using @racket[malt]
or any of its other entry points.

@defproc[(trace-print [v any] [p port]) any]{
  Prints @racket[v] using @racket[display] to the port @racket[p]. If @racket[v] is a dual, only @racket[(ρ v)] is printed.
}

@defform[(check-dual-equal? a b)]{
  A @racket[rackunit] check which checks if @racket[a] and @racket[b] are equal within a tolerance provided by
  the (Racket) parameter @racket[tolerance] according to the following rules.
@itemlist[
@item{If @racket[a] and @racket[b] are both @racket[number?], the check passes if @racket[(abs (- a b))] is less
      than @racket[(tolerance)].}
@item{If @racket[a] is @racket[dual?], the check passes if @racket[(check-dual-equal? (ρ a) b)] passes.}
@item{If @racket[b] is @racket[dual?], the check passes if @racket[(check-dual-equal? a (ρ b))] passes.}
@item{If @racket[a] and @racket[b] are both @racket[list?] and of the same length,
      the check passes if @racket[(check-dual-equal? ea eb)] passes where @racket[ea] and @racket[eb]
      are corresponding members of @racket[a] and @racket[b] respectively.
      }
@item{If @racket[a] and @racket[b] are both @racket[tensor?] and of the same @racket{tlen},
      the check passes if @racket[(check-dual-equal? ea eb)] passes where @racket[ea] and @racket[eb]
      are corresponding elements of @racket[a] and @racket[b] respectively.
      }
@item{If @racket[a] and @racket[b] are @racket[equal?], the check passes}
@item{Otherwise the check failes}
]
}

@defform[(check-ρ-∇ (f arg ...) ans grads)]{
  This check passes if @codeblock{
  (check-dual-equal (f arg ...) ans)
  } would pass and
  @codeblock{
  (check-dual-equal (∇¹ f arg ...) grads)
  } would pass.
}
