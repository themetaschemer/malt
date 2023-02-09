#lang scribble/manual
@title[#:tag "gd-functions"]{Gradient Descent Functions and Hyperparameters}


@declare-exporting[malt]

The following types are used in this section.
@itemlist[
@item{@racket[accompanied?] :  @racket[(listof tensor?)]}
@item{@racket[objective-fn?] :  @racket[(-> theta? tensor?)], as defined in @secref{loss-fns}.}
@item{@racket[id?] :  The identity function}
@item{@racket[inflator?] : @racket[(-> tensor? accompanied?)]}
@item{@racket[deflator?] : @racket[(-> accompanied? tensor?)]}
@item{@racket[updator?] : @racket[(-> accompanied? tensor? accompanied?)]}
@item{@racket[id-updator?] : @racket[(-> tensor? tensor? tensor?)]}
]

Hyperparameters can be given values using @racket[with-hypers] as in @secref{hypers}.

@defproc[(revise [f (-> theta? theta?)] [revs natural?] [theta theta?]) theta?]{
 Returns the result of @racket[(f (f (f ... (f theta))))], where @racket[f] is
 applied @racket[revs] times.
}

@defproc*[([(gradient-descent [inflate id?]
                              [deflate id?]
                              [update  id-updator?]) (-> objective-fn? theta? theta?)]
           [(gradient-descent [inflate inflator?]
                              [deflate deflator?]
                              [update  updator?]) (-> objective-fn? theta? theta?)])]{
  Generates a gradient descent function by accepting three control functions.
  @itemlist[
    @item{@racket[inflate] injects a parameter tensor into an accompanied parameter.}
    @item{@racket[deflate] projects a parameter tensor out of an accompanied parameter.}
    @item{@racket[update] produces a new @racket[accompanied?] from a given accompanied parameter and a gradient tensor.}
  ]

  The generated gradient descent function accepts an objective function and a θ and returns
  a revised θ after @racket[revs] revisions, using gradient descent.
}

@defthing[revs scalar?]{
 Hyperparameter that defines the number of revisions that @racket[gradient-descent] will use.
}

@defthing[alpha scalar?]{
 Hyperparameter that defines the learning rate for the different types of gradient descent functions.
}

@defproc[(naked-gradient-descent [obj? (-> (listof tensor?) scalar?)]
                                 [θ (listof tensor?)]) (listof tensor?)]{
  Gradient descent function where @racket[inflate] and @racket[deflate] are
  the identity function and @racket[update] is @codeblock{
  (λ (pa g)
    (- pa (* alpha g)))
  } where @racket[alpha] is the learning rate hyper parameter.
}

@defproc[(velocity-gradient-descent [obj? (-> (listof tensor?) scalar?)]
                                    [θ (listof tensor?)]) (listof tensor?)]{
  Gradient descent function generated with the following functions
  as the @racket[inflate], @racket[deflate] and @racket[update] @codeblock{
(define velocity-i
  (λ (p)
    (list p (zeroes p))))

(define velocity-d
  (λ (pa)
    (ref pa 0)))

(define velocity-u
  (λ (pa g)
    (let ((v (- (* mu (ref pa 1)) (* alpha g))))
      (list (+ (ref pa 0) v) v))))
}

Here @racket[mu] is the hyperparameter defining the fraction of the velocity from the past revision that
is transferred to the current revision.
}

@defthing[mu scalar?]{
 Hyperparameter that defines the fraction of the gradient that is used to control @racket[velocity-gradient-descent] and
 @racket[adam-gradient-descent].
}

@defproc[(rms-gradient-descent [obj? (-> (listof tensor?) scalar?)]
                               [θ (listof tensor?)]) (listof tensor?)]{
  Gradient descent function generated with the following functions
  as the @racket[inflate], @racket[deflate] and @racket[update] @codeblock{
(define rms-i
  (λ (p)
    (list p (zeroes p))))

(define rms-d
  (λ (pa)
    (ref pa 0)))

(define rms-u
  (λ (pa g)
    (let ((r (smooth beta (ref pa 1) (sqr g))))
      (let ((alpha-hat (/ alpha (+ (sqrt r) epsilon))))
        (list (- (ref pa 0) (* alpha-hat g)) r)))))
}

Here @racket[beta] is the hyperparameter defining the decay rate for smoothing the square of the gradients.
}

@defthing[beta scalar?]{
 Hyperparameter used to smooth gradients in order to control @racket[rms-gradient-descent] and
 @racket[adam-gradient-descent].
}

@defproc[(adam-gradient-descent [obj? (-> (listof tensor?) scalar?)]
                                [θ (listof tensor?)]) (listof tensor?)]{
  Gradient descent function generated with the following functions
  as the @racket[inflate], @racket[deflate] and @racket[update] @codeblock{
(define adam-i
  (λ (p)
    (let ((zeroed (zeroes p)))
      (list p zeroed zeroed))))

(define adam-d
  (λ (pa)
    (ref pa 0)))

(define adam-u
  (λ (pa g)
    (let ((r (smooth beta (ref pa 2) (sqr g))))
      (let ((alpha-hat (/ alpha (+ (sqrt r) epsilon)))
            (v (smooth mu (ref pa 1) g)))
        (list (- (ref pa 0) (* alpha-hat v)) v  r)))))
}

Here @racket[beta] and @racket[mu] are decay rates for smoothing the square of the gradients and the gradient respectively.
}

@defproc[(smooth [decay-rate real?] [average real?] [g real?]) real?]{
 Returns a blending of @racket[average] and @racket[g] using the @racket[decay-rate] as follows @codeblock{
(+ (* decay-rate average)
   (* (- 1.0 decay-rate) g))
 }
}

@defthing[epsilon real?]{
 A numerical stabilizer set to the value @racket[1.0e-8].
}
