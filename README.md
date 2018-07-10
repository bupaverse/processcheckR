
<!-- README.md is generated from README.Rmd. Please edit that file -->
processcheckR
=============

The goal of processcheckR is to support rule-based conformance checking. Currently the following declarative rules can be checked:

-   `exists`: activity occurs n times or more
-   `exists_exactly`: activity occurs exactly n times
-   `absent`: activity does not occur more than n - 1 times
-   `starts`: case starts with activity
-   `ends`: case ends with activity
-   `co_exists`: two activities always exist together
-   `succession`: if activity A happens, B should happen after. If B happens, A should have happened before.
-   `response`: if activity A happens, B should happen after
-   `precedence`: if activity B happens, A should have happend before
-   `responded_existence`: if activity A happens, B should also (have) happen(ed) (i.e. before or after A)

Rules can be checked using the `check_rule` function (see example below). It will create a new logical variable to indicate for which cases the rule holds. The name of the variable can be configured using the `label` argument in `check_rule`.

Installation
------------

You can install processcheckR from github with:

``` r
# install.packages("devtools")
devtools::install_github("gertjanssenswillen/processcheckR")
```

Example
-------

``` r
library(bupaR)
#> Loading required package: edeaR
#> Loading required package: eventdataR
#> Loading required package: processmapR
#> Loading required package: xesreadR
#> Loading required package: processmonitR
#> Loading required package: petrinetR
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
#>     exists
sepsis %>%
  # check if cases starts with "ER Registration"
  check_rule(starts("ER Registration"), label = "r1") %>%
  # check if activities "CRP" and "LacticAcid" occur together
  check_rule(co_exists("CRP","LacticAcid"), label = "r2") %>%
  group_by(r1, r2) %>%
  n_cases() 
#> # A tibble: 4 x 3
#> # Groups:   r1 [?]
#>   r1    r2    n_cases
#>   <lgl> <lgl>   <int>
#> 1 FALSE FALSE      10
#> 2 FALSE TRUE       45
#> 3 TRUE  FALSE     137
#> 4 TRUE  TRUE      858
```
