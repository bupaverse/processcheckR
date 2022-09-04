#' @title Check Multiple Declarative Rules
#'
#' @return
#' An annotated log (of same type as input), where – for every rule – a new column indicates whether the rule holds or not.
#' The name of each rule becomes the name of the column.
#'
#' @param ... Name-rule pairs created by rule functions.
#'
#' @inherit check_rule params description
#'
#' @seealso \code{\link{check_rule}}
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # Check whether MRI Scan is preceded by Blood test, and the case starts with Registration.
#' patients %>%
#'  check_rules(rule1 = precedence("Blood test","MRI SCAN"),
#'              rule2 = starts("Registration"))
#'
#' @export check_rules
check_rules <- function(log, ..., eventlog = deprecated()) {
  UseMethod("check_rules")
}

#' @describeIn check_rules Check rules on a \code{\link[bupaR]{log}}.
#' @export
check_rules.log <- function(log, ..., eventlog = deprecated()) {

  log <- lifecycle_warning_eventlog(log, eventlog)

  rules <- list(...)

  #if(any(duplicated(names(rules)))) {
  #  warning("Some rules have duplicate labels and will be overwritten.")
  #}

  #for(i in seq_along(rules)) {
  #   log <- check_rule(log, rules[[i]], label = names(rules)[i])
  #}

  check_rules_internal(log, rules)
}

check_rules_internal <- function(log, rules) {

  if(any(duplicated(names(rules)))) {
    warning("Some rules have duplicate labels and will be overwritten.")
  }

  for(i in seq_along(rules)) {
    log <- check_rule(log, rules[[i]], label = names(rules)[i])
  }

  return(log)
}
