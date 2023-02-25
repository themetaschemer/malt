# Introduction
Malt is a minimalist deep learning toolkit that is designed to support the book *The Little Learner: A Straight Line to Deep Learning*,
by Daniel P. Friedman and Anurag Mendhekar.

The framework provides for tensors, automatic differentiation,
gradient descent, commonly used loss functions, layer functions and
neural network construction tools.

While it has started off as a pedagogical tool, it is designed with
the future in mind and we are seeking fellow enthusiasts who would be
interested in making it production worthy.

# Prerequisites.

Malt is built on Racket. It can be downloaded [here](https://download.racket-lang.org/). The minimum version this code requires is Racket 8.2.

# Installation
There are two ways to install and use `malt`.
## Install using raco
Malt is distributed as a package that is available directly from [Racket Packages](https://pkgs.racket-lang.org/).
It can be installed like this
```
raco pkg install malt
```
## Install using the Git repository
Another way of installing malt is to clone the git repository and install it as a local package.
For MacOS and Linux:
```
git clone https://github.com/themetaschemer/malt.git
cd malt
make
make install
```

For Windows
```
git clone https://github.com/themetaschemer/malt.git
cd malt
raco pkg install
```

# How the code is structured
There are three main parts to the organization of the code. The first
part is the representation of tensors. This code has three different
representations, `learner`, `nested-tensors`, and `flat-tensors`. The
code for these representations can be found in the respective
directories, `malt/learner`, `malt/nested-tensors`, and `malt/flat-tensors`.
A corresponding `.rkt` file with the same name as the
directory makes the representations available through a single
`require`, e.g. `(require malt/nested-tensors)`. The default representation
is `flat-tensors`, but in some cases, it helps to have other representations.

The `learner` representation is the simplest and is the one described in *Appendix A*.
The `nested-tensors` representation is more complex, but more efficient than `learner` and is described in the first part of *Appendix B*.
The `flat-tensors` representation is the most complex and most efficient and is described briefly in the second part of *Appendix B*.

Each tensor representation is accompanied by matching automatic
differentiation. These can be found in the code directory of the
representation under the sub-directory `autodiff`. Together, these are
followed by all the extended operations which can be found in the
sub-directory `ext-ops`.

The file `impl.rkt` loads the appropriate representation for tensors
at compile time. Using this, the directory `malt/tools` contains
useful tools for building and training deep networks: Hyperparameters,
Normally distributed random numbers and Logging during gradient
descent.

Finally, the directory `malt/malted` contains all the machine learning
specific code developed in *The Little Learner*.

# Using Rackunit
If you're going to be writing unit tests, `check-=` and `check-equal?` will not work
as expected with numbers and tensors. Instead of these, use `check-dual-equal?` which
will check scalars and tensors for equality within a tolerance (currently fixed at
0.0001).


# Running the examples

The examples described in the later chapters of *The Little Learner*, `iris` and `morse`, are located in the
directory `malt/examples`, and can be loaded and run as described here.

## Iris

The Iris example in the book requires the Iris data set to be
loaded. It can be loaded in a racket file like this.
```racket
#lang racket
(require malt)
(require malt/examples/iris)
```

This will make available the following data set
definitions available

```racket
iris-train-xs
iris-train-ys
iris-test-xs
iris-test-ys
```
‍The example can be run with

```racket
(run-iris)
```

This will run a `grid-search` to find a well-trained theta,
and test its accuracy against `iris-test-xs` and `iris-test-ys`.

All the code associated with the example is located in the `malt/examples/iris`
sub-directory.

### ‍A word of advice about Iris.

All neural networks are susceptible to variation depending
upon how they are initialized. For larger networks, this is usually
not a problem because the training process evens out these
variations.

The neural network defined for the Iris example, however,
is very small. This makes it susceptible to much larger variations
because of the randomness in its initialization.

What is important, however, that we arrive at a trained theta that
passes our accuracy thresholds. Running `grid-search` with
`iris-classifier` in order to find a trained theta, consequently, can
produce a different result than what may be described in the book.

Therefore, with the code for the book, we have included the
initial theta that was used when the book went to print. It can be
examined like this

```racket
tll-iris-initial-theta
```
The example on page 266 can then be run like this
```racket
‍(define iris-theta
  (with-hypers ((revs 2000)
                (alpha 0.0002)
                (batch-size 8))
    (naked-gradient-descent
      (sampling-obj (l2-loss iris-classifier)
        iris-train-xs iris-train-ys)
      tll-iris-initial-theta)))
```
The trained theta generated will also have
some variation because of the stochastic nature of
`naked-gradient-descent` using `sampling-obj`. This means that the
accuracy from one trained iris-theta to another varies somewhat as
well.

Readers are encouraged to experiment with `grid-search` as
described in Interlude VI to find the right combination of
hyperparameters for accuracy that is as high possible for the
`iris-test-xs` and `iris-test-ys`.

## Morse
The morse example in the book also requires its own data set to be loaded.
It is done by starting a racket file as follows.

```racket
#lang racket
(require malt)
(require malt/examples/morse)
```

This will load the data set and provide the following training set definitions

```racket
morse-train-xs
morse-train-ys
```
and the following test set definitions
```racket
‍morse-test-xs
morse-test-ys
```

The book describes two different networks, the first being a fully convolutional network (`morse-fcn`) and the second being a Residual network (`morse-residual`).
To train and test `morse-fcn`
```racket
(define fcn-model (train-morse morse-fcn))
(accuracy fcn-model morse-test-xs morse-test-ys)
```
This will display the accuracy of the trained model against the test set.

To train and test morse-residual‍
```racket
(define residual-model (train-morse morse-residual))
(accuracy residual-model morse-test-xs morse-test-ys)
```

This will similarly display the accuracy of the trained model against the test set.
The code in this example is also set up to display progress of the gradient descent
by printing a moving average of the loss in the network at every 20 revisions.
For example,
```racket
(16080 0.072560) [Memory: 139334768][Window size 6]
```
This says that the average of the loss across the last 6 batches at the 16080'th revision was 0.07256, while the system consumed about 139MB of memory. The count of revisions is cumulative, but can be reset by
```racket
(log-malt-reset)
```
Morse examples are currently set up to run 20000 revisions during training.


# Switching tensor representations
In order switch representations, create a
file called `local.cfg` in your local clone of this repository with the following line, and replace `<impl-name>` with one of `learner`, `nested-tensors` or `flat-tensors`. The default implementation is `flat-tensors`.
```racket
(tensor-implementation <representation-name>)
```
Then, rebuild the package

MacOS and Linux:
```shell
make clean && make
```

Windows:
```shell
raco setup --clean malt
raco test -y -p malt
```

# Reference
The documentation for the code is available [here](https://docs.racket-lang.org/malt/index.html). Additional information is
also available at [www.thelittlelearner.com](https://www.thelittlelearner.com).
