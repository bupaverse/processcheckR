
#' Check if activity is present (exists) in a case for exactly n times
#'
#' @param activity Activity too check
#' @param n The exact number of times the activity should be present.
#'
#' @export
#'
exists_exactly <- function(activity, n = 1) {
  if(n < 1) {
    stop("n should be greater than or equal to 1. Use absent to check for absent activities.")
  }
  rule <- list()
  rule$activity <- activity
  rule$n <- n
  class(rule) <- c("conformance_rule","list")
  attr(rule, "type") <- "exists_exactly"
  attr(rule, "checker") <- function(eventlog, rule) {
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
  attr(rule, "label") <- paste0("exists_exactly_", str_replace(activity,"-| ", "_"), "_", n)
  return(rule)
}
