#' Check if activity is present (contained) in a case
#'
#' This rules examines whether the supplied activity is present in a case or not. The argument `n` can be used to set a minimum number of occurences that should be present in each case. Using the function `check_rule`, this information can be added to the event log.
#'
#' @param activity Activity to check. A character vector of length one. Should be an activity of the eventlog supplied with check_rule.
#' @param n The minimum number of times the activity should be present.
#' @family Declarative Rules

#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # Each patient should be registered at least once.
#' patients %>%
#' check_rule(contains("Registration"))
#'
#' # Check whether some patients have received 2 or more blood tests.
#'
#' patients %>%
#' check_rule(contains("Blood test", n = 2))
#'
#' @export
#'
contains <- function(activity, n = 1) {
  if(n < 1) {
    stop("n should be greater than or equal to 1. Use absent to check for absent activities.")
  }
  rule <- list()
  rule$activity <- activity
  rule$n <- n
  class(rule) <- c("conformance_rule","list")
  attr(rule, "type") <- "contains"
  attr(rule, "checker") <- function(eventlog, rule) {


    if(!(rule$activity %in% activity_labels(eventlog))) {
      stop(glue("Activity {rule$activity} not found in eventlog"))
    }

    eventlog %>%
      filter_activity(activities = rule$activity) %>%
      group_by(!!case_id_(eventlog)) %>%
      n_activity_instances() %>%
      filter(n_activity_instances >= rule$n) %>%
      pull(1) -> holds

    eventlog %>%
      mutate(rule_holds = !!case_id_(eventlog) %in% holds) %>%
      return()
  }
  attr(rule, "label") <- paste0("contains_", str_replace(activity,"-| ", "_"), "_", n)
  return(rule)
}
