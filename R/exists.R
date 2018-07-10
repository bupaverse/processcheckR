#' Check if activity is present (exists) in a case
#'
#' @param activity Activity to check
#' @param n The minimum number of times the activity should be present.
#'
#' @export
#'
exists <- function(activity, n = 1) {
  if(n < 1) {
    stop("n should be greater than or equal to 1. Use absent to check for absent activities.")
  }
  rule <- list()
  rule$activity <- activity
  rule$n <- n
  class(rule) <- c("conformance_rule","list")
  attr(rule, "type") <- "exists"
  attr(rule, "checker") <- function(eventlog, rule) {
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
  attr(rule, "label") <- paste0("exists_", str_replace(activity,"-| ", "_"), "_", n)
  return(rule)
}
