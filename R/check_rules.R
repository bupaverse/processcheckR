

#' Check multiple declarative rules.
#'
#' This function can be used to check several rules on event data. It needs an event log and a rule. Rules can be made with the following functions:
#' absent(),
#' and(),
#' contains(),
#' contains_between(),
#' contains_exactly(),
#' ends(),
#' precedence(),
#' response(),
#' responded_existence(),
#' starts(),
#' succession(),
#' xor().
#'
#' @return
#'
#' An annotated event log, where - for every rule - a new column indicates whether the rule holds or not.
#' The name of each rule becomes the name of the column.
#'
#'
#' @param eventlog Eventlog object
#' @param ... Name-rule pairs.
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # check whether MRI Scan is preceded by Blood test, and the case starts with Registration
#' patients %>%
#' check_rules(rule1 = precedence("Blood test","MRI SCAN"),
#'             rule2 = start("Registration"))
#'
#' @export
#'
check_rules <- function(eventlog, ...) {


  rules <- list(...)

  if(any(duplicated(names(rules)))) {
    warning("Some rules have duplicate labels and will be overwritten.")
  }


  for(i in seq_along(rules)) {
     eventlog <- check_rule(eventlog, rules[[i]], label = names(rules)[i])
  }

  eventlog
}


