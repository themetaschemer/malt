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


@defparam[max-tensor-print-length max-length natural? #:value 5]{
 When tensors are printed out using @racket[write], @racket[display], @racket[print], or in the REPL,
 @racket[max-tensor-print-length] determines how many elements are printed after which @racket[...]
 is displayed. If set to 0, the whole tensor is printed.
}

@defparam[raw-tensor-printing? print-raw? boolean? #:value #f]{
 By default, Malt sets the @racket[current-print] to @racket[pretty-print-handler] and customizes the REPL, and
 ports @racket[current-output-port] and @racket[current-error-port] to detect and print tensors and duals in
 a friendlier way. Setting this parameter to @racket[#t] turns off this printing behavior and
 tensors and duals are printed with their raw representation.
}
