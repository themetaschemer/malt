#lang scribble/manual
@title{Tensor functions}

@declare-exporting[malt]

@defproc[(scalar? [s any]) boolean?]{
  Returns @racket[#t] if its argument is a scalar.
}

@defproc[(tensor? [t any]) boolean?]{
  Returns @racket[#t] if its argument is a tensor. A @racket[scalar?] is also a @racket[tensor?].
}

@defproc[(tensor [element tensor?] ...) tensor?]{
  Returns a new tensor formed from the provided @racket[element ...]
  All provided @racket[element]s must have the same shape.
}

@defproc[(shape [t tensor?]) shape?]{
  If @racket[t] is a @racket[scalar?], its shape is the empty list, @racket[(list)].
  Otherwise, its shape is a list starting with the number of elements in @racket[t] followed
  by the shape of the elements of @racket[t].

  For example, the shape of
  @codeblock{
  2.348
  }
  is @racket[(list)].

  The shape of
  @codeblock{
  (tensor 1.0 3.1 2.9)
  }
  is @racket[(list 3)].

  The shape of
  @codeblock{
  (tensor (tensor 1.0 3.1 2.9)
          (tensor 2.8 -6.34 0.98))
  }
  is @racket[(list 2 3)].
}

@defproc[(tlen [t tensor?]) natural?]{
  The number of elements in @racket[t].

  For example,
  @codeblock{
  (tlen (tensor 1.0 3.1 2.9))
  }
  is @racket[3] and

  @codeblock{
  (tlen
    (tensor (tensor 1.0 3.1 2.9)
            (tensor 2.8 -6.34 0.98)))
  }
  is @racket[2].
}

@defproc[(tref [t tensor?] [i (and/c natural? (</c (tlen t)))]) tensor?]{
  Returns the @racket[i]th element of @racket[t].


  For example,
  @codeblock{
  (tref (tensor 1.0 3.1 2.9) 0)
  }
  is @racket[1.0] and

  @codeblock{
  (tref (tensor 1.0 3.1 2.9) 1)
  }
  is @racket[3.1] and
}

@defproc[(list->tensor [lst (listof tensor?)]) tensor?]{
  Constructs a tensor using the members of @racket[lst] as elements. All members
  of @racket[lst] must have the same @racket[shape].
}

@defproc[(build-tensor [s shape?] [filler (-> (listof natural?) scalar?)]) tensor?]{
  Constructs a new tensor with shape @racket[s] where each scalar is determined by invoking
  the function @racket[filler] with a list representing the position of the scalar. The list is a sequence
  of indices of the recursively nested tensor elements.

  @codeblock{
  (build-tensor (list 2 3) (λ (idx) (+ (ref idx 0) (ref idx 1))))
  }
  produces
  @codeblock{
  (tensor (tensor 0 1 2)
          (tensor 1 2 3))
  }
}

@defproc[(rank [t tensor?]) natural?]{
  Returns the length of the @racket[shape] of @racket[t].
}

@defproc[(reshape [s shape?] [t tensor?]) tensor?]{
  Attempts to rearrange the nesting of scalars in @racket[t] so that the resulting shape
  of the rearranged tensor is @racket[s]. Fails if the rearrangement is not possible, i.e.,
  when the product of the numbers in @racket[s] does not equal the total number of scalars in @racket[t].
}

@defproc[(trefs [t tensor?][b (list (and/c natural? (</c (tlen t))))]) tensor?]{
  Here @racket[b] is a list of indices into @racket[t]. Returns a tensor
  consisting of the elements @racket[(tref t i)] for each @racket[i] in @racket[b]
  in the order of appearance in @racket[b].
}