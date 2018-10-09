#' Check for exclusiveness of two activities
#'
#' If activity A exists, Activity B should not exist, and vice versa.
#'
#' @param activity_a Activity A. A character vector of length one. This should be an activity of the event log supplied to  `check_rule`.
#' @param activity_b Activity B. A character vector of length one. This should be an activity of the event log supplied to  `check_rule`.
#' @family Declarative Rules
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # A patient should not receive both an X-Ray and MRI Scan
#' patients %>%
#' check_rule(xor("X-Ray","MRI SCAN"))
#' @export
#'
xor <- function(activity_a, activity_b) {
  rule <- list()
  rule$activity_a <- activity_a
  rule$activity_b <- activity_b
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "xor"

  attr(rule, "checker") <- function(eventlog, rule) {


    if(!(rule$activity_a %in% activity_labels(eventlog))) {
      stop(glue("Activity {rule$activity_a} not found in eventlog"))
    }

    if(!(rule$activity_b %in% activity_labels(eventlog))) {
      stop(glue("Activity {rule$activity_b} not found in eventlog"))
    }

    eventlog %>%
      group_by_case() %>%
      mutate(rule_holds = !(any(!!activity_id_(eventlog) == rule$activity_a) &
                             any(!!activity_id_(eventlog) == rule$activity_b))) %>%
      ungroup_eventlog()
  }
  attr(rule, "label") <- paste0("xor_",
                                str_replace(activity_a, "-| ", "_"),
                                "_",
                                str_replace(activity_b, "-| ", "_"))
  return(rule)
}
