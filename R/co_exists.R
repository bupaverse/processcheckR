#' Check for co-existence of two activities
#'
#' If activity A exists, Activity B should also exist, and vice versa.
#'
#' @param activity_a Activity A
#' @param activity_b Activity B
#'
#' @export
#'
co_exists <- function(activity_a, activity_b) {
  rule <- list()
  rule$activity_a <- activity_a
  rule$activity_b <- activity_b
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "co_exists"

  attr(rule, "checker") <- function(eventlog, rule) {
    eventlog %>%
      group_by_case() %>%
      mutate(rule_holds = (any(!!activity_id_(eventlog) == rule$activity_a) &
                             any(!!activity_id_(eventlog) == rule$activity_b)) |
               (!any(!!activity_id_(eventlog) == rule$activity_a) &
                  !any(!!activity_id_(eventlog) == rule$activity_b))) %>%
      ungroup_eventlog()
  }
  attr(rule, "label") <- paste0("co_exists_",
                                str_replace(activity_a, "-| ", "_"),
                                "_",
                                str_replace(activity_b, "-| ", "_"))
  return(rule)
}
