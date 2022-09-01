#' @title Contains Between
#'
#' @description Check if the specified activity is present (contained) in a case between the minimum and maximum number of times.
#'
#' The `contains_between` rule examines whether the supplied `activity` is present in a case for a certain interval of times.
#' The arguments `min` and `max` can be used to specify the allowed interval of occurences.
#'
#' @param activity \code{\link{character}}: The activity to check. This should be an activity of the log supplied to \code{\link{check_rule}}.
#' @param min \code{\link{numeric}} (default `1`): The minimum number of times the activity should be present (inclusive).
#' Should be greater than or equal to `0`.
#' @param max \code{\link{numeric}} (default `1`): The maximum number of times the activity should be present (inclusive).
#' Should be greater than or equal to `min`.
#'
#' @family Declarative Rules
#'
#' @seealso \code{\link{absent}},\code{\link{contains}},\code{\link{contains_exactly}}
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # A patients should have between 0 and 4 blood tests (including 0 and 4).
#' patients %>%
#'  check_rule(contains_between("Blood test", min = 0, max = 4))
#'
#' @export
contains_between <- function(activity, min = 1, max = 1) {

  if(min < 0) {
    stop("min should be greater than or equal to 0.")
  }
  if(max < min) {
    stop("max should be greater or equal than min.")
  }

  rule <- list()
  rule$activity <- activity
  rule$min <- min
  rule$max <- max
  class(rule) <- c("conformance_rule","list")
  attr(rule, "type") <- "contains_between"
  attr(rule, "checker") <- contains_between_checker
  attr(rule, "label") <- paste0("contains_between_", str_replace(activity,"-| ", "_"), "_", min, "_", max)

  return(rule)
}

contains_between_checker <- function(log, rule) {
  UseMethod("contains_between_checker")
}

contains_between_checker.log <- function(log, rule) {

  check_activity_in_log(rule$activity, log)

  log %>%
    filter_activity(activities = rule$activity) %>%
    group_by_case() %>%
    n_activity_instances() %>%
    right_join(tibble(!!case_id(log) := case_labels(log)), by = case_id(log)) %>%
    replace_na(list("n_activity_instances" = 0)) %>%
    #mutate(n_activity_instances = ifelse(is.na(n_activity_instances), 0, n_activity_instances)) %>%
    filter(between(.data[["n_activity_instances"]], rule$min, rule$max)) %>%
    pull(1) -> holds

  log %>%
    mutate(rule_holds = .data[[case_id(log)]] %in% holds)
}