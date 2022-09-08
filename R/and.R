#' @title AND
#'
#' @description Check for co-existence of two activities.
#'
#' The`and` rule checks whether two activities both occur in a case (or are both absent).
#' If `activity_a` exists, `activity_b` should also exist, and vice versa.
#'
#' @param activity_a \code{\link{character}}: Activity A. This should be an activity of the log supplied to \code{\link{check_rule}}.
#' @param activity_b \code{\link{character}}: Activity B. This should be an activity of the log supplied to \code{\link{check_rule}}.
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # Check that if a patients is registered, he's also checked-out, and vice versa.
#' patients %>%
#'  check_rule(and("Registration","Check-out"))
#'
#' @family Declarative Rules
#'
#' @seealso \code{\link{xor}}
#'
#' @export
and <- function(activity_a, activity_b) {

  rule <- list()
  rule$activity_a <- activity_a
  rule$activity_b <- activity_b
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "and"
  attr(rule, "checker") <- and_checker
  attr(rule, "label") <- paste0("and_",
                                       str_replace(activity_a, "-| ", "_"),
                                       "_",
                                       str_replace(activity_b, "-| ", "_"))

  return(rule)
}

and_checker <- function(log, rule) {
  UseMethod("and_checker")
}

and_checker.log <- function(log, rule) {

  check_activity_in_log(rule$activity_a, log)
  check_activity_in_log(rule$activity_b, log)

  log %>%
    group_by_case() %>%
    mutate(rule_holds = (any(.data[[activity_id(log)]] == rule$activity_a) &
                         any(.data[[activity_id(log)]] == rule$activity_b)) |
                        (!any(.data[[activity_id(log)]] == rule$activity_a) &
                         !any(.data[[activity_id(log)]] == rule$activity_b))) %>%
    ungroup_eventlog()
}

and_checker.grouped_log <- function(log, rule) {

  bupaR:::apply_grouped_fun(log, and_checker.log, rule, .ignore_groups = TRUE, .keep_groups = TRUE, .returns_log = TRUE)
}