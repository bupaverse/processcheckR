#' Check if an activity is absent from a case
#'
#' @param activity The activity too check
#' @param n n-1 is the allowed number of occurences of the activity. E.g. n = 1 means the activity should be absent, n = 2 means it is allowed to occur once.
#'
#' @export
#'
absent <- function(activity, n = 1) {
  if(n < 1) {
    stop("n should be greater than or equal to 1. Use exists to check for present activities.")
  }
  rule <- list()
  rule$activity <- activity
  rule$n <- n
  class(rule) <- c("conformance_rule","list")
  attr(rule, "type") <- "absent"
  attr(rule, "checker") <- function(eventlog, rule) {
    eventlog %>%
      filter_activity(activities = rule$activity) %>%
      group_by(!!case_id_(eventlog)) %>%
      n_activity_instances() %>%
      filter(n_activity_instances >= rule$n) %>%
      pull(1) -> holds

    eventlog %>%
      mutate(rule_holds = !(!!case_id_(eventlog) %in% holds)) %>%
      return()
  }
  attr(rule, "label") <- paste0("absent_", str_replace(activity,"-| ", "_"), "_", n)
  return(rule)
}
