
#' Check if cases end with an activity.
#'
#' @param activity The end activity. Character vector of length one. This should be an activity of the event log supplied to  `check_rule`.
#' @family Declarative Rules

#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#'
#' # A patient's last activity should be the Check-out
#' patients %>%
#' check_rule(ends("Check-out"))
#' @export
#'
ends <- function(activity) {
  rule <- list()
  rule$activity <- activity
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "ends"
  attr(rule, "checker") <- function(eventlog, rule) {

    if(!(rule$activity %in% activity_labels(eventlog))) {
      stop(glue("Activity {rule$activity} not found in eventlog"))
    }

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
