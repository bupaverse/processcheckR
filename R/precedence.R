#' @title Precedence
#'
#' @description Check for precedence between two activities.
#'
#' If `activity_b` occured, it should be preceded by `activity_a` in the same case.
#'
#' @inherit and params
#'
#' @family Relation rules
#'
#' @seealso \code{\link[edeaR]{filter_precedence}},\code{\link{responded_existence}},\code{\link{response}},\code{\link{succession}}
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # A MRI Scan should be preceeded by a Blood test.
#'
#' patients %>%
#'  check_rule(precedence("Blood test","MRI SCAN"))
#'
#' @export
precedence <- function(activity_a, activity_b) {

  rule <- list()
  rule$activity_a <- activity_a
  rule$activity_b <- activity_b
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "precedence"
  attr(rule, "checker") <- precedence_checker
  attr(rule, "label") <- paste0("precedence_",
                                       str_replace(activity_a, "-| ", "_"),
                                       "_",
                                       str_replace(activity_b, "-| ", "_"))

  return(rule)
}

precedence_checker <- function(log, rule) {
  UseMethod("precedence_checker")
}

precedence_checker.log <- function(log, rule) {

  check_activity_in_log(rule$activity_a, log)
  check_activity_in_log(rule$activity_b, log)

  log %>%
    filter_precedence(antecedents = rule$activity_a,
                      consequents = rule$activity_b,
                      precedence_type = "eventually_follows") %>%
    case_labels() -> holds

  log %>%
    group_by_case() %>%
    mutate(rule_holds = (.data[[case_id(log)]] %in% holds) |
                        !(any(.data[[activity_id(log)]] == rule$activity_b))) %>%
    ungroup_eventlog()
}

precedence_checker.grouped_log <- function(log, rule) {

  bupaR:::apply_grouped_fun(log, precedence_checker.log, rule, .ignore_groups = TRUE, .keep_groups = TRUE, .returns_log = TRUE)
}