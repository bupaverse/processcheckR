#' @title Starts
#'
#' @description Check if cases start with the specified activity.
#'
#' @param activity \code{\link{character}}: The start activity. This should be an activity of the log supplied to \code{\link{check_rule}}.
#'
#' @family Declarative Rules
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # Each patients should first be registered.
#' patients %>%
#'  check_rule(starts("Registration"))
#'
#' @export
starts <- function(activity) {

  rule <- list()
  rule$activity <- activity
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "starts"
  attr(rule, "checker") <- starts_checker
  attr(rule, "label") <- paste0("starts_with_", str_replace(activity, "-| ", "_"))

  return(rule)
}

starts_checker <- function(log, rule) {
  UseMethod("starts_checker")
}

starts_checker.eventlog <- function(log, rule) {

  check_activity_in_log(rule$activity, log)

  log %>%
    group_by_case() %>%
    arrange(.data[[timestamp(log)]]) %>%
    mutate(rule_holds = first(.data[[activity_id(log)]] == rule$activity)) %>%
    ungroup_eventlog()
}

starts_checker.activitylog <- function(log, rule) {

  starts_checker.eventlog(bupaR::to_eventlog(log), rule) %>%
    bupaR::to_activitylog() %>%
    drop_generated_columns()
}