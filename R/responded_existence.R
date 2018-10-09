#' Check for responded existence between two activity
#'
#' If activity A occurs in a case, activity B should also occur (before or after).
#'
#' @param activity_a Activity A. A character vector of length one. This should be an activity of the event log supplied to  `check_rule`.
#' @param activity_b Activity B. A character vector of length one. This should be an activity of the event log supplied to  `check_rule`.
#'
#'
#' @family Declarative Rules
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#'
#' # When a Blood test occurs, a MRI Scan should also have
#' # happened for this patient (before or after the test).
#'
#' patients %>%
#' check_rule(responded_existence("Blood test","MRI SCAN"))
#'
#' @export
#'
responded_existence <- function(activity_a, activity_b) {
  rule <- list()
  rule$activity_a <- activity_a
  rule$activity_b <- activity_b
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "respondend_existence"
  attr(rule, "checker") <- function(eventlog, rule) {


    if(!(rule$activity_a %in% activity_labels(eventlog))) {
      stop(glue("Activity {rule$activity_a} not found in eventlog"))
    }

    if(!(rule$activity_b %in% activity_labels(eventlog))) {
      stop(glue("Activity {rule$activity_b} not found in eventlog"))
    }

    eventlog %>%
      group_by_case() %>%
      mutate(rule_holds = (any(!!activity_id_(eventlog) == rule$activity_a) &
                             any(!!activity_id_(eventlog) == rule$activity_b)) |
               !any(!!activity_id_(eventlog) == rule$activity_a)) %>%
      ungroup_eventlog()
  }
  attr(rule, "label") <- paste0("responded_existence_",
                                str_replace(activity_a, "-| ", "_"),
                                "_",
                                str_replace(activity_b, "-| ", "_"))
  return(rule)
}
