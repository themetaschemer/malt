#lang scribble/manual
@title[#:tag "loss-fns"]{Loss Functions}

@declare-exporting[malt]

Loss functions are curried and have the following type:
@codeblock{
(target-fn? . -> . expectant-fn?)
}
where a @racket[target-fn?] expects a tensor first, and then a @racket[theta?], and returns a @racket[tensor?].
An @racket[expectant-fn?] expects @racket[xs] and @racket[ys] which
are two tensors representing a subset of the dataset, and returns
an @racket[objective-fn?].


These are defined as follows
@itemlist[
@item{@racket[target-fn?] :  @racket[(-> tensor? (-> theta? tensor?))]}
@item{@racket[expectant-fn?] :  @racket[(-> tensor? tensor? objective-fn?)]}
@item{@racket[objective-fn?] : @racket[(-> theta? tensor?)]}
]

The tensor returned from an @racket[objective-fn?] must have rank 1,
and its @racket[tlen] should be the same as the number of elements in
@racket[xs].

The following loss functions are available in @racket[malt].

@defproc[(((l2-loss [target (-> tensor? (-> theta? tensor?))]) (xs tensor?) (ys tensor)) [θ theta?]) tensor?]{
  Implements the SSE loss function. @codeblock{
  (let ((pred-ys ((target xs) theta)))
    (sum
      (sqr
        (- ys pred-ys))))
  }
}

@defproc[(((cross-entropy-loss [target (-> tensor? (-> theta? tensor?))]) (xs tensor?) (ys tensor)) [θ theta?]) tensor?]{
  Implements the cross-entropy loss function. @codeblock{
  (let ((pred-ys ((target xs) theta))
        (num-classes (ref (reverse (shape ys)) 0)))
    (* -1
      (/ (dot-product ys (log pred-ys))
         num-classes)))
  }
}

@defproc[(((kl-loss [target (-> tensor? (-> theta? tensor?))]) (xs tensor?) (ys tensor)) [θ theta?]) tensor?]{
  Implements the KL-divergence loss function. @codeblock{
  (let ((pred-ys ((target xs) theta)))
    (sum (* pred-ys (log (/ pred-ys ys)))))
  }
}
