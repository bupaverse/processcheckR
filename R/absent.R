#' Check if an activity is absent from a case
#'
#' The `absent` rule can be used to check whether an activity is absent in a case or not. The `n` parameter can be configured to create a different level of _absence_.
#' When n = 1, an activity is not allowed to occur even a single time. The maximum number of times it is allowed to occur is `n-1`.
#'
#'
#' @param activity The activity to check. Character vector of length one.
#' @param n n-1 is the allowed number of occurences of the activity. E.g. n = 1 means the activity should be absent, n = 2 means it is allowed to occur once.
#'
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
#' check_rule(absent("Blood test", n = 2))
#'
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

    if(!(rule$activity %in% activity_labels(eventlog))) {
      stop(glue("Activity {rule$activity} not found in eventlog"))
    }

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
