#' Check for precedence between two activities.
#'
#' If activity B occured, it should be preceded by activity A
#'
#' @param activity_a Activity A
#' @param activity_b Activity B
#'
#' @export
#'
precedence <- function(activity_a, activity_b) {
  rule <- list()
  rule$activity_a <- activity_a
  rule$activity_b <- activity_b
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "precedence"
  attr(rule, "checker") <- function(eventlog, rule) {
    eventlog %>%
      filter_precedence(antecedents = rule$activity_a,
                        consequents = rule$activity_b,
                        precedence_type = "eventually_follows") %>%
      case_labels -> holds

    eventlog %>%
      group_by_case() %>%
      mutate(rule_holds = (!!case_id_(eventlog) %in% holds) |
               !(any(!!activity_id_(eventlog) == rule$activity_b))) %>%
      ungroup_eventlog() %>%
      return()

  }
  attr(rule, "label") <- paste0("precedence_",
                                str_replace(activity_a, "-| ", "_"),
                                "_",
                                str_replace(activity_b, "-| ", "_"))
  return(rule)
}
