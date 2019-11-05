

#' Filter using declarative rules.
#'
#' This function can be used to filter event data using declaritive rules. It needs an event log and a rule. Rules can be made with the following functions:
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
#' A filtered event log.
#'
#'
#' @param eventlog Eventlog object
#' @param ... rules
#' @examples
#' library(eventdataR)
#'
#' # Filter where Blood test precedes MRI SCAN and Registration is the start of the case.
#' filter_rules(patients, precedence("Blood test","MRI SCAN"),
#'              starts("Registration"))
#'
#' @export
#'
filter_rules <- function(eventlog, ...) {


  rules <- list(...)
  names(rules) <- paste0("filtering_rule", seq_along(rules))

  for(i in seq_along(rules)) {
    eventlog <- check_rule(eventlog, rules[[i]], label = names(rules)[i])
  }

  eventlog %>%
    filter_at(vars(starts_with("filtering_rule")), all_vars(. == T)) %>%
    re_map(mapping(eventlog))
  }


