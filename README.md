
<!-- README.md is generated from README.Rmd. Please edit that file -->

# satpt

<!-- badges: start -->
<!-- badges: end -->

The goal of satpt is to identify the saturation point of multinomial
responses from a survey using standard errors of sample proportions

## Installation

You can install the development version of `satpt` like so:

``` r
#install.packages("remotes")
remotes::install_github("deboonstra/satpt")
```

## Useage

``` r
data(ein)
satpt::satpt(ein$x, ein$y)
satpt::satpt(y ~ x, data = ein)
```
