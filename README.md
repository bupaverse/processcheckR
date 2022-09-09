
<!-- README.md is generated from README.Rmd. Please edit that file -->

# processcheckR <a href="https://bupaverse.github.io/processcheckR/"><img src="man/figures/logo.png" align="right" height="50" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/processcheckR)](https://CRAN.R-project.org/package=processcheckR)
[![R-CMD-check](https://github.com/bupaverse/processcheckR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bupaverse/processcheckR/actions/workflows/R-CMD-check.yaml)
[![Codecov](https://codecov.io/gh/bupaverse/processcheckR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/bupaverse/processcheckR?branch=master)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

The goal of **processcheckR** is to support rule-based conformance
checking. Currently the following declarative rules can be checked:

Cardinality rules:

- `absent`: activity does not occur more than `n - 1` times,
- `contains`: activity occurs `n` times or more,
- `contains_between`: activity occurs between `n` and `m` times,
- `contains_exactly`: activity occurs exactly `n` times.

Ordering rules:

- `starts`: case starts with activity,
- `ends`: case ends with activity,
- `succession`: if activity A happens, B should happen after. If B
  happens, A should have happened before,
- `response`: if activity A happens, B should happen after,
- `precedence`: if activity B happens, A should have happened before,
- `responded_existence`: if activity A happens, B should also (have)
  happen(ed) (i.e. before or after A).

Exclusiveness:

- `and`: two activities always exist together,
- `xor`: two activities are not allowed to exist together.

Rules can be checked using the `check_rule` and `check_rules` functions
(see example below). It will create a new logical variable to indicate
for which cases the rule holds. The name of the variable can be
configured using the `label` argument in `check_rule`.

## Installation

You can install **processcheckR** from
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("processcheckR")
```

### Development Version

You can also install the latest (stable) development version with
bugfixes and new features directly from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("bupaverse/processcheckR")
```

## Example

``` r
library(bupaR)
#> 
#> Attaching package: 'bupaR'
#> The following object is masked from 'package:stats':
#> 
#>     filter
#> The following object is masked from 'package:utils':
#> 
#>     timestamp
library(processcheckR)
#> 
#> Attaching package: 'processcheckR'
#> The following object is masked from 'package:base':
#> 
#>     xor
sepsis %>%
  # Check if cases starts with "ER Registration".
  check_rule(starts("ER Registration"), label = "r1") %>%
  # Check if activities "CRP" and "LacticAcid" occur together.
  check_rule(and("CRP","LacticAcid"), label = "r2") %>%
  group_by(r1, r2) %>%
  n_cases() 
#> # A tibble: 4 × 3
#>   r1    r2    n_cases
#>   <lgl> <lgl>   <int>
#> 1 FALSE FALSE      10
#> 2 FALSE TRUE       45
#> 3 TRUE  FALSE     137
#> 4 TRUE  TRUE      858
```
