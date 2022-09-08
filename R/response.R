#' @title Response
#'
#' @description Check for response between two activities.
#'
#' If `activity_a` is executed, it should be (eventually) followed by `activity_b`.
#'
#' @inherit and params
#'
#' @family Relation rules
#'
#' @seealso \code{\link{precedence}},\code{\link{responded_existence}},\code{\link{succession}}
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # A blood test should eventually be followed by Discuss Results.
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

  # log %>%
  #  filter_precedence(antecedents = rule$activity_a,
  #                    consequents = rule$activity_b,
  #                    precedence_type = "eventually_follows") %>%
  #  case_labels() -> holds

  sequence <- paste(rule$activity_a, rule$activity_b, sep = ",")

  log %>%
    filter_activity(activities = c(rule$activity_a, rule$activity_b)) %>%
    case_list(.keep_trace_list = TRUE) %>%
    rowwise() %>%
    mutate(trace = list(str_flatten(trace_list[trace_list != c(trace_list[-1], FALSE)], ","))) %>%
    ungroup() %>%
    mutate(sequence_count = str_count(trace, sequence),
           activity_a_count = str_count(trace, rule$activity_a),
           holds = if_else(sequence_count == activity_a_count, TRUE, FALSE)) %>%
    filter(holds) %>%
    pull(case_id(log)) -> holds

  log %>%
    group_by_case() %>%
    mutate(rule_holds = (.data[[case_id(log)]] %in% holds) |
                        !(any(.data[[activity_id(log)]] == rule$activity_a))) %>%
    ungroup_eventlog()
}

response_checker.grouped_log <- function(log, rule) {

  bupaR:::apply_grouped_fun(log, response_checker.log, rule, .ignore_groups = TRUE, .keep_groups = TRUE, .returns_log = TRUE)
}
