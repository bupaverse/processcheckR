#' @title XOR
#'
#' @description Check for exclusiveness of two activities.
#'
#' If `activity_a` exists, `activity_b` should not exist, and vice versa.
#'
#' @inherit and params
#'
#' @family Exclusiveness rules
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # A patient should not receive both an X-Ray and MRI Scan.
#' patients %>%
#'  check_rule(xor("X-Ray","MRI SCAN"))
#'
#' @export
xor <- function(activity_a, activity_b) {

  rule <- list()
  rule$activity_a <- activity_a
  rule$activity_b <- activity_b
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "xor"
  attr(rule, "checker") <- xor_checker
  attr(rule, "label") <- paste0("xor_",
                                       str_replace(activity_a, "-| ", "_"),
                                       "_",
                                       str_replace(activity_b, "-| ", "_"))

  return(rule)
}

xor_checker <- function(log, rule) {
  UseMethod("xor_checker")
}

xor_checker.log <- function(log, rule) {

  check_activity_in_log(rule$activity_a, log)
  check_activity_in_log(rule$activity_b, log)

  log %>%
    group_by_case() %>%
    mutate(rule_holds = !(any(.data[[activity_id(log)]] == rule$activity_a) &
                          any(.data[[activity_id(log)]] == rule$activity_b))) %>%
    ungroup_eventlog()
}

xor_checker.grouped_log <- function(log, rule) {

  bupaR:::apply_grouped_fun(log, xor_checker.log, rule, .ignore_groups = TRUE, .keep_groups = TRUE, .returns_log = TRUE)
}
