#' @title Contains
#'
#' @description Check if the specified activity is present (contained) in a case.
#'
#' The `contains` rule examines whether the supplied `activity` is present in a case or not.
#' The argument `n` can be used to set a minimum number of occurences that should be present in each case.
#'
#' @param activity \code{\link{character}}: The activity to check. This should be an activity of the log supplied to \code{\link{check_rule}}.
#' @param n \code{\link{numeric}} (default \code{1}): The minimum number of times the activity should be present.
#' Should be greater than or equal to 1. Use \code{\link{absent}} instead to check for absent (i.e. `n = 0`) activities.
#'
#' @family Declarative Rules
#'
#' @seealso \code{\link{absent}},\code{\link{contains_between}},\code{\link{contains_exactly}}
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # Each patient should be registered at least once.
#' patients %>%
#'  check_rule(contains("Registration"))
#'
#' # Check whether some patients have received 2 or more blood tests.
#' patients %>%
#'  check_rule(contains("Blood test", n = 2))
#'
#' @export
contains <- function(activity, n = 1) {

  if(n < 1) {
    stop("n should be greater than or equal to 1. Use absent to check for absent activities.")
  }

  rule <- list()
  rule$activity <- activity
  rule$n <- n
  class(rule) <- c("conformance_rule","list")
  attr(rule, "type") <- "contains"
  attr(rule, "checker") <- contains_checker
  attr(rule, "label") <- paste0("contains_", str_replace(activity,"-| ", "_"), "_", n)

  return(rule)
}

contains_checker <- function(log, rule) {
  UseMethod("contains_checker")
}

contains_checker.log <- function(log, rule) {

  check_activity_in_log(rule$activity, log)

  log %>%
    filter_activity(activities = rule$activity) %>%
    group_by_case() %>%
    n_activity_instances() %>%
    filter(n_activity_instances >= rule$n) %>%
    pull(1) -> holds

  log %>%
    mutate(rule_holds = .data[[case_id(log)]] %in% holds)
}