#' @title Absent
#'
#' @description Check if the specified activity is absent from a case.
#'
#' The `absent` rule can be used to check whether an activity is absent in a case or not. The `n` parameter can be configured to create a different level of _absence_.
#' When `n = 0`, an activity is not allowed to occur even a single time. The maximum number of times it is allowed to occur is `n`.
#'
#' @param activity \code{\link{character}}: The activity to check. This should be an activity of the log supplied to \code{\link{check_rule}}.
#' @param n \code{\link{numeric}} (default \code{0}): The allowed number of occurences of the activity, e.g. `n = 0` means the activity should be absent,
#' `n = 1` means it is allowed to occur once.
#'
#' @family Declarative Rules
#'
#' @seealso \code{\link{contains}},\code{\link{contains_between}},\code{\link{contains_exactly}}
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # Check for which patients the activity "MRI SCAN" is absent.
#' patients %>%
#'  check_rule(absent("MRI SCAN"))
#'
#' # Check for which patients the activity "Blood test" occurs maximum a single time,
#' # but not 2 times or more.
#' patients %>%
#'  check_rule(absent("Blood test", n = 1))
#'
#' @export
absent <- function(activity, n = 0) {

  if(n < 0) {
    stop("n should be greater than or equal to 0.")
  }

  rule <- list()
  rule$activity <- activity
  rule$n <- n
  class(rule) <- c("conformance_rule","list")
  attr(rule, "type") <- "absent"
  attr(rule, "checker") <- absent_checker
  attr(rule, "label") <- paste0("absent_", str_replace(activity,"-| ", "_"), "_", n)

  return(rule)
}

absent_checker <- function(log, rule) {
  UseMethod("absent_checker")
}

absent_checker.log <- function(log, rule) {

  check_activity_in_log(rule$activity, log)

  log %>%
    filter_activity(activities = rule$activity) %>%
    group_by_case() %>%
    n_activity_instances() %>%
    filter(n_activity_instances > rule$n) %>%
    pull(1) -> holds_not

  log %>%
    mutate(rule_holds = .data[[case_id(log)]] %in% holds_not)
}
