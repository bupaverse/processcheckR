#' @title Precedence
#'
#' @description Check for precedence between two activities.
#'
#' If `activity_b` occured, it should be preceded by `activity_a` in the same case, i.e., if `B` was executed, it could not
#' have been executed before `A` was executed. For example, the trace `[A,C,B,B,A]` satisfies the `precedence` relation.
#'
#' @inherit and params
#'
#' @family Ordering rules
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

  # log %>%
  #   filter_precedence(antecedents = rule$activity_a,
  #                     consequents = rule$activity_b,
  #                     precedence_type = "eventually_follows") %>%
  #   case_labels() -> holds

  pattern <- paste(rule$activity_a, rule$activity_b, sep = ",")

  log %>%
    case_list(.keep_trace_list = TRUE) %>%
    mutate("holds" := vapply(.data[["trace_list"]], FUN = precedence_satisfied, FUN.VALUE = logical(1), rule$activity_a, rule$activity_b, pattern)) %>%
    filter(.data[["holds"]]) %>%
    pull(case_id(log)) -> holds

  log %>%
    group_by_case() %>%
    mutate(rule_holds = (.data[[case_id(log)]] %in% holds) |
                        !(any(.data[[activity_id(log)]] == rule$activity_b))) %>%
    ungroup_eventlog()
}

precedence_checker.grouped_log <- function(log, rule) {

  bupaR:::apply_grouped_fun(log, precedence_checker.log, rule, .ignore_groups = TRUE, .keep_groups = TRUE, .returns_log = TRUE)
}

# Checks whether B was preceded by A. B does not have to be executed immediately after A,
# and another B can be executed between between the first A and subsequent Bs.
precedence_satisfied <- function(trace, a, b, pattern) {

  rle_trace <- rle(trace[trace %in% c(a, b)])$values
  flatten <- stringi::stri_flatten(rle_trace, collapse = ",")
  sequence_count <- stringi::stri_count_fixed(flatten, pattern)
  b_count <- length(which(rle_trace == b))

  if (sequence_count == b_count) {
    return(TRUE)
  }

  return(FALSE)
}