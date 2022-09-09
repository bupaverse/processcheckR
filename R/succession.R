#' @title Succession
#'
#' @description Check for succession between two activities.
#'
#' `succession` checks the bi-directional execution order of `activity_a` and `activity_b`, i.e., both \code{\link{response}}
#' and \code{\link{precedence}} relations have to hold: every `A` has to be (eventually) followed by `B`, and there has to be
#' an `A` before every `B`. For example, the trace `[A,C,A,B,B]` satisfies the `succession` relation.
#'
#' @inherit and params
#'
#' @family Ordering rules
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
  attr(rule, "type") <- "succession"
  attr(rule, "checker") <- succession_checker
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

  # log %>%
  #   filter_precedence(antecedents = rule$activity_a,
  #                     consequents = rule$activity_b,
  #                     precedence_type = "eventually_follows") %>%
  #   case_labels() -> holds

  pattern <- paste(rule$activity_a, rule$activity_b, sep = ",")

  log %>%
    case_list(.keep_trace_list = TRUE) %>%
    mutate("precedence_holds" := vapply(.data[["trace_list"]], FUN = precedence_satisfied, FUN.VALUE = logical(1), rule$activity_a, rule$activity_b, pattern),
           "response_holds" := vapply(.data[["trace_list"]], FUN = response_satisfied, FUN.VALUE = logical(1), rule$activity_a, rule$activity_b, pattern)) %>%
    filter(.data[["precedence_holds"]] & .data[["response_holds"]]) %>%
    pull(case_id(log)) -> holds

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
