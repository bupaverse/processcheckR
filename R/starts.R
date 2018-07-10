#'  Check if cases start with an activity
#'
#' @param activity The start activity. Character vector of length one
#'
#' @export
#'
starts <- function(activity) {
  rule <- list()
  rule$activity <- activity
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "starts"
  attr(rule, "checker") <- function(eventlog, rule) {
    eventlog %>%
      group_by(!!case_id_(eventlog)) %>%
      arrange(!!timestamp_(eventlog)) %>%
      mutate(rule_holds = first(!!activity_id_(eventlog) == rule$activity)) %>%
      ungroup_eventlog() %>%
      return()
  }
  attr(rule, "label") <- paste0("starts_with_", str_replace(activity, "-| ", "_"))
  return(rule)
}













