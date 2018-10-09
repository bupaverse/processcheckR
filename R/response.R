#' Check for response between two activities
#'
#' If activity A is executed, it should be eventually followed by activity B.
#'
#' @param activity_a Activity A. A character vector of length one. This should be an activity of the event log supplied to  `check_rule`.
#' @param activity_b Activity B. A character vector of length one. This should be an activity of the event log supplied to  `check_rule`.
#'
#'
#' @family Declarative Rules
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # A blood test should eventually be followed by Discuss Results
#'
#' patients %>%
#' check_rule(response("Blood test","Discuss Results"))
#'
#' @export
#'
response <- function(activity_a, activity_b) {
  rule <- list()
  rule$activity_a <- activity_a
  rule$activity_b <- activity_b
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "response"
  attr(rule, "checker") <- function(eventlog, rule) {


    if(!(rule$activity_a %in% activity_labels(eventlog))) {
      stop(glue("Activity {rule$activity_a} not found in eventlog"))
    }

    if(!(rule$activity_b %in% activity_labels(eventlog))) {
      stop(glue("Activity {rule$activity_b} not found in eventlog"))
    }

    eventlog %>%
      filter_precedence(antecedents = rule$activity_a,
                        consequents = rule$activity_b,
                        precedence_type = "eventually_follows") %>%
      case_labels -> holds

    eventlog %>%
      group_by_case() %>%
      mutate(rule_holds = (!!case_id_(eventlog) %in% holds) |
               !(any(!!activity_id_(eventlog) == rule$activity_a))) %>%
      ungroup_eventlog() %>%
      return()

  }
  attr(rule, "label") <- paste0("response_",
                                str_replace(activity_a, "-| ", "_"),
                                "_",
                                str_replace(activity_b, "-| ", "_"))
  return(rule)
}
