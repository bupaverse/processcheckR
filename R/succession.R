#' @title Succession
#'
#' @description Check for succession between two activities.
#'
#' If `activity_a` happens, it should be eventually followed by `activity_b`.
#' If `activity_b` happens, it should be preceded by `activity_a`.
#'
#' @inherit and params
#'
#' @family Relation rules
#'
#' @seealso \code{\link{precedence}},\code{\link{responded_existence}},\code{\link{response}}
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # Blood test should always happen before a MRI Scan,
#' # and both should happen when one of them happens.
#' patients %>%
#'  check_rule(succession("Blood test","MRI SCAN"))
#'
#' @export
succession <- function(activity_a, activity_b) {

  rule <- list()
  rule$activity_a <- activity_a
  rule$activity_b <- activity_b
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "precedence"
  attr(rule, "checker") <- function(eventlog, rule) {



    if(!(rule$activity_a %in% activity_labels(eventlog))) {
      stop(glue("Activity {rule$activity_a} not found in eventlog"))
    }

    if(!(rule$activity_b %in% activity_labels(eventlog))) {
      stop(glue("Activity {rule$activity_b} not found in eventlog"))
    }

    eventlog %>%
      filter_precedence(antecedents = rule$activity_a,
                        consequents = rule$activity_b,
                        precedence_type = "eventually_follows") %>%
      case_labels -> holds

    eventlog %>%
      group_by_case() %>%
      mutate(rule_holds = (!!case_id_(eventlog) %in% holds) | (
        !(any(!!activity_id_(eventlog) == rule$activity_b)) &
          !(any(!!activity_id_(eventlog) == rule$activity_a))
      )) %>%
      ungroup_eventlog() %>%
      return()

  }
  attr(rule, "label") <- paste0("succession_",
                                str_replace(activity_a, "-| ", "_"),
                                "_",
                                str_replace(activity_b, "-| ", "_"))
  return(rule)
}

succession_checker <- function(log, rule) {
  UseMethod("succession_checker")
}

succession_checker.log <- function(log, rule) {

  check_activity_in_log(rule$activity_a, log)
  check_activity_in_log(rule$activity_b, log)

  log %>%
    filter_precedence(antecedents = rule$activity_a,
                      consequents = rule$activity_b,
                      precedence_type = "eventually_follows") %>%
    case_labels() -> holds

  log %>%
    group_by_case() %>%
    mutate(rule_holds = (.data[[case_id(log)]] %in% holds) |
                        (!(any(.data[[activity_id(log)]] == rule$activity_b)) &
                         !(any(.data[[activity_id(log)]] == rule$activity_a)))) %>%
    ungroup_eventlog()
}

succession_checker.grouped_log <- function(log, rule) {

  bupaR:::apply_grouped_fun(log, succession_checker.log, rule, .ignore_groups = TRUE, .keep_groups = TRUE, .returns_log = TRUE)
}
