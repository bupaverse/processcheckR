#' @title Response
#'
#' @description Check for response between two activities.
#'
#' If `activity_a` is executed, it should be (eventually) followed by `activity_b`.
#'
#' @inherit and params
#'
#' @family Declarative Rules
#'
#' @seealso \code{\link{precedence}},\code{\link{responded_existence}},\code{\link{succession}}
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # A blood test should eventually be followed by Discuss Results
#'
#' patients %>%
#'  check_rule(response("Blood test","Discuss Results"))
#'
#' @export
response <- function(activity_a, activity_b) {

  rule <- list()
  rule$activity_a <- activity_a
  rule$activity_b <- activity_b
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "response"
  attr(rule, "checker") <- response_checker
  attr(rule, "label") <- paste0("response_",
                                       str_replace(activity_a, "-| ", "_"),
                                       "_",
                                       str_replace(activity_b, "-| ", "_"))

  return(rule)
}

response_checker <- function(log, rule) {
  UseMethod("response_checker")
}

response_checker.log <- function(log, rule) {

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
                        !(any(.data[[activity_id(log)]] == rule$activity_a))) %>%
    ungroup_eventlog()
}

response_checker.grouped_log <- function(log, rule) {

  bupaR:::apply_grouped_fun(log, response_checker.log, rule, .ignore_groups = TRUE, .keep_groups = TRUE, .returns_log = TRUE)
}
