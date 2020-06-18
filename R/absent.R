#' Check if an activity is absent from a case
#'
#' The `absent` rule can be used to check whether an activity is absent in a case or not. The `n` parameter can be configured to create a different level of _absence_.
#' When n = 0, an activity is not allowed to occur even a single time. The maximum number of times it is allowed to occur is `n`.
#'
#'
#' @param activity The activity to check. Character vector of length one.
#' @param n n is the allowed number of occurences of the activity. E.g. n = 0 means the activity should be absent, n = 1 means it is allowed to occur once.

#' @family Declarative Rules
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # Check for which patients the activity "MRI SCAN" is absent.
#' patients %>%
#' check_rule(absent("MRI SCAN"))
#'
#' # Check for which patients the activity "Blood test"
#' # occurs maximum a single time, but not 2 times or more.
#' patients %>%
#' check_rule(absent("Blood test", n = 1))
#'
#'
#' @export
#'
absent <- function(activity, n = 0) {
  if(n < 0) {
    stop("n should be greater than or equal to 1.")
  }
  rule <- list()
  rule$activity <- activity
  rule$n <- n
  class(rule) <- c("conformance_rule","list")
  attr(rule, "type") <- "absent"
  attr(rule, "checker") <- function(eventlog, rule) {

    if(!(rule$activity %in% activity_labels(eventlog))) {
      stop(glue("Activity {rule$activity} not found in eventlog"))
    }

    eventlog %>%
      filter_activity(activities = rule$activity) %>%
      group_by(!!case_id_(eventlog)) %>%
      n_activity_instances() %>%
      filter(n_activity_instances > rule$n) %>%
      pull(1) -> holds_not

    eventlog %>%
      mutate(rule_holds = !(!!case_id_(eventlog) %in% holds_not)) %>%
      return()
  }
  attr(rule, "label") <- paste0("absent_", str_replace(activity,"-| ", "_"), "_", n)
  return(rule)
}
