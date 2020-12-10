
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rcf

<!-- badges: start -->

<!-- badges: end -->

rcf is a highly simplified (and sort of crude) fully R based
implementation of some features of Julie Tibshirani’s, Susan Athey’s,
Stefan Wager’s and the grf lab team’s causal forest functionality
(<https://grf-labs.github.io/grf/>). This was always meant to be a fun
project to get stuck into the workings of the algorithm and try to get
to grips with the theory behind it (as far as I understood it) a bit
better by just trying to build the functions from scratch. I wouldn’t
suggest using this (for any high stakes work at least) for the following
reasons:

1.  this is really just an experimental implementation of my
    understanding of the grf causal forest algorithm
2.  the functionality is much more limited than what grf offers
3.  it’s still a bit slow

That being said when running these causal forest functions and the grf
implementation on the same data the results are quite similar (same
ballpark when looking at correlations between conditional average
treatment effect estimates produced by both implementations). So if you
prefer working in R and want to experiment around with some causal
forest code hopefully this is somewhat useful.

##### References and Reading:

Rina Friedberg, Julie Tibshirani, Susan Athey, and Stefan Wager. Local
Linear Forests. 2018 (<https://arxiv.org/abs/1807.11408>)

Stefan Wager and Susan Athey. Estimation and Inference of Heterogeneous
Treatment Effects using Random Forests. Journal of the American
Statistical Association, 113(523), 2018.
(<https://arxiv.org/abs/1510.04342>)

Susan Athey and Stefan Wager. Estimating Treatment Effects with Causal
Forests: An Application. Observational Studies, 5, 2019.
(<https://arxiv.org/abs/1902.07409>)

Susan Athey, Julie Tibshirani and Stefan Wager. Generalized Random
Forests. Annals of Statistics, 47(2), 2019.
(<https://arxiv.org/abs/1610.01271>)

## Installation

You can install the development version of rcf from
(<https://github.com/>) with:

``` r
devtools::install_github("till-tietz/rcf")
```

I’ll add some R examples and some math here soon.
