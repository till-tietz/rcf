
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rcf

<!-- badges: start -->

[![R-CMD-check](https://github.com/till-tietz/rcf/workflows/R-CMD-check/badge.svg)](https://github.com/till-tietz/rcf/actions)
<!-- badges: end -->

rcf is a simplified fully R based implementation of causal forests based
on some features of Julie Tibshirani’s, Susan Athey’s, Stefan Wager’s
and the grf lab team’s causal forest functionality
(<https://grf-labs.github.io/grf/>). This was always meant to be a fun
project to get stuck into the workings of the algorithm and try to get
to grips with the theory behind it (as far as I understood it) a bit
better by just trying to build the functions from scratch. I wouldn’t
suggest using this (for any high stakes work at least) for the following
reasons:

1.  this is really just an experimental implementation of my
    understanding of the grf causal forest algorithm
2.  the functionality is much more limited than what grf offers

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

## Usage

``` r
# generate some data
data <- as.data.frame(do.call(cbind, replicate(10, rnorm(100), simplify=FALSE)))
data[["treat"]] <- rbinom(nrow(data),1,0.5)
vars <- colnames(data)[1:(ncol(data)-2)]

# set up parallel processing 
future::plan("multisession")

# build causal forest 
cf <- rcf::causal_forest(n_trees = 1000, data = data, outcome = "V10",
                         covariates = vars, treat = "treat", minsize = 5,
                         alpha = 0.05, feature_fraction = 0.5, sample_fraction = 0.5,
                         honest_split = TRUE, honesty_fraction = 0.5)

# predict cates
cate <- rcf::predict_causal_forest(data = data, cf = cf, predict_oob = TRUE)
```

predict\_causal\_forest returns a data.frame of observation ids and cate
estimates

| obs |      cate |
| --: | --------: |
|   1 | 0.0747438 |
|   2 | 0.1442352 |
|   3 | 0.1347309 |
|   4 | 0.1796071 |
|   5 | 0.1534446 |
|   6 | 0.1485099 |
|   7 | 0.0513095 |
|   8 | 0.1540657 |
|   9 | 0.0592893 |
|  10 | 0.1090305 |

variable\_importance generates a data.frame of variable importance
metrics

``` r
var_importance <- rcf::variable_importance(cf = cf, covariates = vars, n = 4, d = 2)
```

| variable | importance         |
| :------- | :----------------- |
| V6       | 0.126494773519164  |
| V7       | 0.102355400696864  |
| V2       | 0.100794425087108  |
| V3       | 0.100682926829268  |
| V8       | 0.0985087108013937 |
| V9       | 0.0984529616724739 |
| V4       | 0.0913728222996516 |
| V1       | 0.082787456445993  |
| V5       | 0.0765993031358885 |

## Performance compared to grf

We’ll build 500 grf and rcf causal forests respectively and compare the
means of their cate predictions for each observation.

``` r
grf_sim <- function(x){
  grf <- grf::causal_forest(X = data[,vars], Y = data[,"V10"], W = data[,"treat"],
                            num.trees = 1000,mtry = 5, min.node.size = 5,
                            honesty = TRUE, honesty.fraction = 0.5, alpha = 0.05)
  
  results <- as.data.frame(t(predict(grf)[["predictions"]]))
  return(results)
}

results_grf <- furrr::future_map_dfr(1:500, ~grf_sim(.x), .progress = TRUE)%>%
  dplyr::summarise_all(mean)%>%
  t()

rcf_sim <- function(x){
  cf <- rcf::causal_forest(n_trees = 1000, data = data, outcome = "V10",
                           covariates = vars, treat = "treat", minsize = 5,
                           alpha = 0.05, feature_fraction = 0.5, honest_split = TRUE,
                           honesty_fraction = 0.5)
  
  results <- as.data.frame(t(rcf::predict_causal_forest(data = data, cf = cf, predict_obb = TRUE)[["cate"]]))
  return(results)
}

results_rcf <- furrr::future_map_dfr(1:500, ~rcf_sim(.x), .progress = TRUE)%>%
  dplyr::summarise_all(mean)%>%
  t()
```

The rcf cate predictions match those generated by grf relatively well.

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

## Performance compared to other methods

We’ll test the performance of the rcf causal forest against a linear
regression and knn approach to estimating heterogeneous treatment
effects. We’ll use a simulated data set with explicit treatment effect
heterogeneity across two variables.

![](man/figures/performance.png)

## Methodology

### Explicitly Optimizing Heterogeneity

rfc serves as an estimator for conditional average treatment effects by
explicitly optimizing on treatment effect heterogeneity. This is
achieved by recursively splitting a sample such as to maximize the
following quantity of interest:

![max(MSD\_p) = (*{p*{i1}} - *{p*{i2}})^2 - (1 -
)*{j=1}<sup>{2}(s\_{p\_{ij\_{treat}}}</sup>2 +
s*{p\_{ij\_{control}}}^2)](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+max%28MSD_p%29+%3D+%5Calpha%28%5Ctau_%7Bp_%7Bi1%7D%7D+-+%5Ctau_%7Bp_%7Bi2%7D%7D%29%5E2+-+%281+-+%5Calpha%29%5Cfrac%7B1%7D%7Bn%7D%5Csum_%7Bj%3D1%7D%5E%7B2%7D%28s_%7Bp_%7Bij_%7Btreat%7D%7D%7D%5E2+%2B+s_%7Bp_%7Bij_%7Bcontrol%7D%7D%7D%5E2%29%0A%0A)

Mean squared difference in treatment effects \(\tau\) across sub-samples
created by a set of all possible partitions of a sample \(P\) minus the
sum of variances in outcomes for treatment and control units summed
across sub-samples. The two components of the equation are weighted by
the parameter \(\alpha\).

### Algorithm

1.  Draw a sample of `size = n_data (feature_fraction)` without
    replacement
2.  If honest\_split is `TRUE`, split this sample into a tree fitting
    sample of `size = n_sample (1 – honesty_fraction)` and an honest
    estimation sample of `size = n_sample(honesty_fraction)`
3.  Draw a sample of covariates of `size = n_covariates
    (feature_fraction)`
4.  Find unique values of all sampled covariates in the tree fitting
    sample
5.  Split the tree fitting sample at each unique value and assess if
    there are `n > minsize` treatment and control observations in each
    sub\_sample created by the split (keep only those split points where
    the minsize requirement is met)
6.  For each valid split point compute the above quantity of interest
    (variance of treatment effects across sub-samples minus sum of
    variances in outcomes for treatment and control units in each
    sub-sample). Choose the split that maximizes this value.
7.  Keep recursively splitting each sub-sample of the tree fitting
    sample until no split can satisfy the minsize requirement. The tree
    is fully grown at this point.
8.  Push the honest estimation sample down the tree (i.e. subset the
    honest estimation sample according to the splitting rules of the
    tree grown with the tree fitting sample).
9.  Repeat 1-8 `n_trees` times.
10. Push a test sample down each tree in the forest (i.e. subset the
    test sample according to the splitting rules of each tree in the
    forest). For each observation, record the honest sample observations
    in each terminal leaf it falls into. Compute CATE for each
    observation using its honest sample observations neighbours.

### Variable Importance

Variable Importance is computed as a weighted sum of how often a
variable was split at depth k within a tree.

![imp(x\_j) =
)](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+imp%28x_j%29+%3D+%5Cfrac%7B%5Csum_%7Bk%3D1%7D%5En%5Cleft%5B%5Cfrac%7B%5Csum_%7Ball%5C%3Btrees%7Dnumber%5C%2Cof%5C%2Cdepth%5C%2Ck%5C%2Csplits%5C%2Con%5C%2Cx_j%7D%7B%5Csum_%7Ball%5C%3Btrees%7Dtotal%5C%2Cnumber%5C%2Cof%5C%2Cdepth%5C%2Ck%5C%2Csplits%7D%5Cright%5Dk%5E%7B-d%7D%7D%7B%5Csum_%7Bk%3D1%7D%5Enk%5E%7B-d%7D%7D%29%0A%0A)

Where:  
![n =
maximum,depth,to,consider,splits,at](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+n+%3D+maximum%5C%2Cdepth%5C%2Cto%5C%2Cconsider%5C%2Csplits%5C%2Cat%0A%0A)  

![d =
decay,paramater](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle+d+%3D+decay%5C%2Cparamater%0A%0A)
