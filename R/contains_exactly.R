
#' Check if activity is present (contained) in a case for exactly n times
#'
#' This rules examines whether the supplied activity is present in a case for an exact number of times.
#'
#' @param activity Activity too check. A character vector of length one. This should be an activity of the event log supplied to `check_rule`.
#' @param n The exact number of times the activity should be present.
#'
#' @family Declarative Rules
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # Each patient should have exactly one registration activity instance.
#'
#' patients %>%
#' check_rule(contains_exactly("Registration", n = 1))
#'
#' @export
#'
contains_exactly <- function(activity, n = 1) {
  if(n < 1) {
    stop("n should be greater than or equal to 1. Use absent to check for absent activities.")
  }
  rule <- list()
  rule$activity <- activity
  rule$n <- n
  class(rule) <- c("conformance_rule","list")
  attr(rule, "type") <- "contains_exactly"
  attr(rule, "checker") <- function(eventlog, rule) {

    if(!(rule$activity %in% activity_labels(eventlog))) {
      stop(glue("Activity {rule$activity} not found in eventlog"))
    }

    eventlog %>%
      filter_activity(activities = rule$activity) %>%
      group_by(!!case_id_(eventlog)) %>%
      n_activity_instances() %>%
      filter(n_activity_instances == rule$n) %>%
      pull(1) -> holds

    eventlog %>%
      mutate(rule_holds = !!case_id_(eventlog) %in% holds) %>%
      return()
  }
  attr(rule, "label") <- paste0("contains_exactly_", str_replace(activity,"-| ", "_"), "_", n)
  return(rule)
}
