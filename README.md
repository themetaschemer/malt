# Introduction
Malt is a minimalist deep learning toolkit that is designed to support
the book @italic{The Little Learner}, by Daniel P. Friedman and
Anurag Mendhekar.

The framework provides for tensors, automatic differentiation,
gradient descent, commonly used loss functions, layer functions and
neural network construction tools.

While it has started off as a pedagogical tool, it is designed with
the future in mind and we are seeking fellow enthusiasts who would be
interested in making it production worthy.

# Preequisites.

Malt is built on Racket. It can be downloaded [here](https://download.racket-lang.org/).

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
```
git clone https://github.com/themetaschemer/malt.git
cd malt
make
make install
```
# Switching Tensor Implementations
Malt is designed with multiple tensor implementations (intended for different purposes---some pedagogical, some for efficiency). These
implementations can be switched when Malt has been installed as a Git repository. In order switch implementations, create a
file called `local.cfg` in your local clone of the repository with the following line, and replace `<impl-name>` with one of `learner`, `nested-tensors` or `flat-tensors`. The default implementation is `flat-tensors`.
```
(tensor-implementation <impl-name>)
```
Then, rebuild the package
```
make clean && make
```

# Reference
The documentation for the code is available [here](https://docs.racket-lang.org/malt/index.html). Additional information is
also available at [www.thelittlelearner.com](https://www.thelittlelearner.com).
