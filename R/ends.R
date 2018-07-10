
#' Check if cases end with an activity
#'
#' @param activity The end activity. Character vector of length one.
#'
#' @export
#'
ends <- function(activity) {
  rule <- list()
  rule$activity <- activity
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "ends"
  attr(rule, "checker") <- function(eventlog, rule) {
    eventlog %>%
      group_by(!!case_id_(eventlog)) %>%
      arrange(!!timestamp_(eventlog)) %>%
      mutate(rule_holds = last(!!activity_id_(eventlog) == rule$activity)) %>%
      ungroup_eventlog() %>%
      return()
  }
  attr(rule, "label") <- paste0("ends_with_", str_replace(activity, "-| ", "_"))
  return(rule)
}
