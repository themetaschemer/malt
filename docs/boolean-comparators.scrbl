#lang scribble/manual
@title{Boolean comparison functions}

@declare-exporting[malt]

These functions are used for comparison between @racket[scalar?]s.

@deftogether[(
@defproc[(=-0-0 (a scalar?) (b scalar?)) boolean?]
@defproc[(= (a scalar?) (b scalar?)) boolean?])]{
 Returns #t if @racket[(ρ a)] is equal to @racket[(ρ b)].
 When a @racket[no-override] entry point is used, @racket[=] is not redefined to @racket[=-0-0].
}

@deftogether[(
@defproc[(<-0-0 (a scalar?) (b scalar?)) boolean?]
@defproc[(< (a scalar?) (b scalar?)) boolean?])]{
 Returns #t if @racket[(ρ a)] is less than @racket[(ρ b)].
 When a @racket[no-override] entry point is used, @racket[<] is not redefined to @racket[<-0-0].
}

@deftogether[(
@defproc[(>-0-0 (a scalar?) (b scalar?)) boolean?]
@defproc[(> (a scalar?) (b scalar?)) boolean?])]{
 Returns #t if @racket[(ρ a)] is greater than @racket[(ρ b)].
 When a @racket[no-override] entry point is used, @racket[>] is not redefined to @racket[>-0-0].
}

@deftogether[(
@defproc[(<=-0-0 (a scalar?) (b scalar?)) boolean?]
@defproc[(<= (a scalar?) (b scalar?)) boolean?])]{
 Returns #t if @racket[(ρ a)] is less than or equal to @racket[(ρ b)].
 When a @racket[no-override] entry point is used, @racket[<=] is not redefined to @racket[<=-0-0].
}

@deftogether[(
@defproc[(>=-0-0 (a scalar?) (b scalar?)) boolean?]
@defproc[(>= (a scalar?) (b scalar?)) boolean?])]{
 Returns #t if @racket[(ρ a)] is greater than or equal to @racket[(ρ b)].
 When a @racket[no-override] entry point is used, @racket[>=] is not redefined to @racket[>=-0-0].
}
