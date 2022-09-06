#' @title Contains Exactly
#'
#' @description Check if the specified activity is present (contained) in a case for exactly `n` times.
#'
#' The `contains_exactly` rule examines whether the supplied `activity` is present in a case for an exact number of `n` times.
#'
#' @param activity \code{\link{character}}: The activity to check. This should be an activity of the log supplied to \code{\link{check_rule}}.
#' @param n \code{\link{numeric}} (default `1`): The exact number of times the activity should be present.
#' Should be greater than or equal to `1`. Use \code{\link{absent}} instead to check for absent (i.e. `n = 0`) activities.
#'
#' @family Declarative Rules
#'
#' @seealso \code{\link{absent}},\code{\link{contains}},\code{\link{contains_between}}
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # Each patient should have exactly one registration activity instance.
#' patients %>%
#'  check_rule(contains_exactly("Registration", n = 1))
#'
#' @export
contains_exactly <- function(activity, n = 1) {

  if(n < 1) {
    stop("n should be greater than or equal to 1. Use absent to check for absent activities.")
  }

  rule <- list()
  rule$activity <- activity
  rule$n <- n
  class(rule) <- c("conformance_rule","list")
  attr(rule, "type") <- "contains_exactly"
  attr(rule, "checker") <- contains_exactly_checker
  attr(rule, "label") <- paste0("contains_exactly_", str_replace(activity,"-| ", "_"), "_", n)

  return(rule)
}

contains_exactly_checker <- function(log, rule) {
  UseMethod("contains_exactly_checker")
}

contains_exactly_checker.log <- function(log, rule) {

  check_activity_in_log(rule$activity, log)

  log %>%
    filter_activity(activities = rule$activity) %>%
    group_by_case() %>%
    n_activity_instances() %>%
    filter(.data[["n_activity_instances"]] == rule$n) %>%
    pull(1) -> holds

  log %>%
    mutate(rule_holds = .data[[case_id(log)]] %in% holds)
}

contains_exactly_checker.grouped_log <- function(log, rule) {

  bupaR:::apply_grouped_fun(log, contains_exactly_checker.log, rule, .ignore_groups = TRUE, .keep_groups = TRUE, .returns_log = TRUE)
}