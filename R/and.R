#' Check for co-existence of two activities
#'
#' The`and` rule checks whether two activities both occur in a case (or are both absent).
#' If activity A exists, Activity B should also exist, and vice versa.
#'
#' @param activity_a Activity A. A character vector of length one. This should be an activity of the event log supplied to  `check_rule`.
#' @param activity_b Activity B. A character vector of length one. This should be an activity of the event log supplied to  `check_rule`.
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # Check that if a patients is registered, he's also checked-out, and vice versa.
#' patients %>%
#' check_rule(and("Registration","Check-out"))
#'
#' @family Declarative Rules
#'
#' @export
#'
and <- function(activity_a, activity_b) {
  rule <- list()
  rule$activity_a <- activity_a
  rule$activity_b <- activity_b
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "and"

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
               (!any(!!activity_id_(eventlog) == rule$activity_a) &
                  !any(!!activity_id_(eventlog) == rule$activity_b))) %>%
      ungroup_eventlog()
  }
  attr(rule, "label") <- paste0("and_",
                                str_replace(activity_a, "-| ", "_"),
                                "_",
                                str_replace(activity_b, "-| ", "_"))
  return(rule)
}
