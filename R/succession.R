#' Check succession between two activities
#'
#' If activity A happens, it should be eventually followed by activity B.
#' If activity B happens, it should be preceded by activity A.
#'
#' @param activity_a Activity A
#' @param activity_b Activity B
#'
#' @export
#'
succession <- function(activity_a, activity_b) {
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
      mutate(rule_holds = (!!case_id_(eventlog) %in% holds) | (
        !(any(!!activity_id_(eventlog) == rule$activity_b)) &
          !(any(!!activity_id_(eventlog) == rule$activity_a))
      )) %>%
      ungroup_eventlog() %>%
      return()

  }
  attr(rule, "label") <- paste0("succession_",
                                str_replace(activity_a, "-| ", "_"),
                                "_",
                                str_replace(activity_b, "-| ", "_"))
  return(rule)
}
