#' @title Responded Existence
#'
#' @description Check for responded existence between two activities.
#'
#' If `activity_a` occurs in a case, `activity_b` should also occur (before or after).
#'
#' @inherit and params
#'
#' @family Relation rules
#'
#' @seealso \code{\link{precedence}},\code{\link{response}},\code{\link{succession}}
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # When a Blood test occurs, a MRI Scan should also have
#' # happened for this patient (before or after the test).
#'
#' patients %>%
#'  check_rule(responded_existence("Blood test","MRI SCAN"))
#'
#' @export
responded_existence <- function(activity_a, activity_b) {

  rule <- list()
  rule$activity_a <- activity_a
  rule$activity_b <- activity_b
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "responded_existence"
  attr(rule, "checker") <- responded_existence_checker
  attr(rule, "label") <- paste0("responded_existence_",
                                       str_replace(activity_a, "-| ", "_"),
                                       "_",
                                       str_replace(activity_b, "-| ", "_"))

  return(rule)
}

responded_existence_checker <- function(log, rule) {
  UseMethod("responded_existence_checker")
}

responded_existence_checker.log <- function(log, rule) {

  check_activity_in_log(rule$activity_a, log)
  check_activity_in_log(rule$activity_b, log)

  log %>%
    group_by_case() %>%
    mutate(rule_holds = (any(.data[[activity_id(log)]] == rule$activity_a) &
                         any(.data[[activity_id(log)]] == rule$activity_b)) |
                        !any(.data[[activity_id(log)]] == rule$activity_a)) %>%
    ungroup_eventlog()
}

responded_existence_checker.grouped_log <- function(log, rule) {

  bupaR:::apply_grouped_fun(log, responded_existence_checker.log, rule, .ignore_groups = TRUE, .keep_groups = TRUE, .returns_log = TRUE)
}