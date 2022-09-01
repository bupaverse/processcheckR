#' @title processcheckR - Check rules in event data
#'
#' @description  Tools to check declarative rules in event logs.

#' @docType package
#' @keywords internal
#' @name processcheckR
#'
## usethis namespace: start
#' @import dplyr
#' @import bupaR
#' @importFrom edeaR filter_activity
#' @importFrom edeaR filter_precedence
#' @importFrom stringr str_replace
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
#' @importFrom tidyr replace_na
## usethis namespace: end

globalVariables(c(".", ":="))
"_PACKAGE"
NULL
