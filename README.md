
<!-- README.md is generated from README.Rmd. Please edit that file -->

# satpt

<!-- badges: start -->
<!-- badges: end -->

The goal of `satpt` *R* package is to identify the saturation point of
multinomial responses from a survey using standard errors of sample
proportions

## Installation

You can install the development version of `satpt` by

``` r
#install.packages("remotes")
remotes::install_github("deboonstra/satpt")
```

## Usage

### Basic usage

For basic usage of `satpt` simply specify the responses of the survey in
`y` and the data collection period information in `by`, as seen below.

``` r
data(ein)
satpt::satpt(y = ein$q2, by = ein$wave)
```

    Saturation achieved?  Yes 
    Overall Sample Proportions and Standard Errors
    ==============================================
                y
    Statistics   Not at all   Once Rarely Sometimes  Often
      Proportion     0.2531 0.0375 0.3688    0.2656 0.0750
      SE             0.0172 0.0075 0.0191    0.0175 0.0104

### Dimension names specification

When specification of the dimension names of the contigency table,
sample proportions, and standard error is wanted, use one of these two
methods.

#### Method 1

``` r
res <- satpt::satpt(
  y = ein$q2,
  by = ein$wave,
  dimnames = c("Responses to survey", "Collection period")
)
res$counts # accessing contingency table
```

                     Responses to survey
    Collection period Not at all Once Rarely Sometimes Often
                    1         85   15    109        88    27
                    2         37    5     59        34    11
                    3         40    4     68        48    10

#### Method 2

``` r
res <- satpt::satpt(
  y = ein$q2,
  by = ein$wave,
  dimnames = c("by" = "Collection period", "y" = "Responses to survey")
)
res$phat # accessing sample proportions table
```

                     Responses to survey
    Collection period Not at all       Once    Rarely Sometimes      Often
                    1  0.2623457 0.04629630 0.3364198 0.2716049 0.08333333
                    2  0.2534247 0.03424658 0.4041096 0.2328767 0.07534247
                    3  0.2352941 0.02352941 0.4000000 0.2823529 0.05882353
