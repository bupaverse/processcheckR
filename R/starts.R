#'  Check if cases start with an activity
#'
#' @param activity The start activity. Character vector of length one. This should be an activity of the event log supplied to  `check_rule`.
#' @family Declarative Rules
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # Each patients should first be registered.
#' patients %>%
#' check_rule(starts("Registration"))
#'
#' @export
#'
starts <- function(activity) {
  rule <- list()
  rule$activity <- activity
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "starts"
  attr(rule, "checker") <- function(eventlog, rule) {

    if(!(rule$activity %in% activity_labels(eventlog))) {
      stop(glue("Activity {rule$activity} not found in eventlog"))
    }


    eventlog %>%
      as.data.frame() %>%
      group_by(!!case_id_(eventlog)) %>%
      arrange(!!timestamp_(eventlog)) %>%
      mutate(rule_holds = first(!!activity_id_(eventlog) == rule$activity)) %>%
      ungroup() %>%
      re_map(mapping(eventlog)) %>%
      return()
  }
  attr(rule, "label") <- paste0("starts_with_", str_replace(activity, "-| ", "_"))
  return(rule)
}













