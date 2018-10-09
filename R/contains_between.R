
#' Check if activity is present (contained) in a case between  min and max number of times
#'
#' This rules examines whether the supplied activity is present in a case for a certain interval of times.
#' The arguments `min` and `max` can be used to specify the allowed interval of occurences.
#'
#' @param activity Activity too check. Character vector of length one. This should be an activity of the event log supplied with `check_rule`
#' @param min The minimum number of times the activity should be present.
#' @param max The maximum number of times the activity should be present.
#'
#' @family Declarative Rules
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#'
#' # A patients should have between 0 and 4 blood tests (including 0 and 4).
#' patients %>%
#' check_rule(contains_between("Blood test", min = 0, max = 4))
#'
#' @export
#'
contains_between <- function(activity, min = 1, max = 1) {
  if(max < min) {
    stop("max should be greater or equal than min")
  }
  rule <- list()
  rule$activity <- activity
  rule$min <- min
  rule$max <- max
  class(rule) <- c("conformance_rule","list")
  attr(rule, "type") <- "contains_between"
  attr(rule, "checker") <- function(eventlog, rule) {

    if(!(rule$activity %in% activity_labels(eventlog))) {
      stop(glue("Activity {rule$activity} not found in eventlog"))
    }

    eventlog %>%
      filter_activity(activities = rule$activity) %>%
      group_by(!!case_id_(eventlog)) %>%
      n_activity_instances() %>%
      filter(between(n_activity_instances, rule$min, rule$max)) %>%
      pull(1) -> holds

    eventlog %>%
      mutate(rule_holds = !!case_id_(eventlog) %in% holds) %>%
      return()
  }
  attr(rule, "label") <- paste0("contains_between_", str_replace(activity,"-| ", "_"), "_", min,"_",max)
  return(rule)
}
