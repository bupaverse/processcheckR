#' @title Ends
#'
#' @description Check if cases end with the specified activity.
#'
#' @param activity \code{\link{character}}: The end activity. This should be an activity of the log supplied to \code{\link{check_rule}}.
#'
#' @family Declarative Rules
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # A patient's last activity should be the Check-out
#' patients %>%
#'  check_rule(ends("Check-out"))
#'
#' @export
ends <- function(activity) {

  rule <- list()
  rule$activity <- activity
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "ends"
  attr(rule, "checker") <- ends_checker
  attr(rule, "label") <- paste0("ends_with_", str_replace(activity, "-| ", "_"))

  return(rule)
}

ends_checker <- function(log, rule) {
  UseMethod("ends_checker")
}

ends_checker.eventlog <- function(log, rule) {

  if(!(rule$activity %in% activity_labels(log))) {
    stop(glue("Activity {rule$activity} not found in log."))
  }

  log %>%
    group_by(.data[[case_id(log)]]) %>%
    arrange(.data[[timestamp(log)]]) %>%
    mutate(rule_holds = last(.data[[activity_id(log)]] == rule$activity)) %>%
    ungroup_eventlog()
}

ends_checker.grouped_eventlog <- function(log, rule) {

  log %>%
    ungroup_eventlog() %>%
    ends_checker.eventlog(rule)
}

ends_checker.activitylog <- function(log, rule) {

  ends_checker.eventlog(bupaR::to_eventlog(log), rule) %>%
    bupaR::to_activitylog()
}

ends_checker.grouped_activitylog <- function(log, rule) {

  log %>%
    ungroup_eventlog() %>%
    ends_checker.activitylog(rule)
}