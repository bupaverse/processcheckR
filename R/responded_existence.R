#' Check for responded existence between two activity
#'
#' If activity A occurs in a case, activity B should also occur (before or after).
#'
#' @param activity_a Activity A
#' @param activity_b Activity B
#'
#' @export
#'
responded_existence <- function(activity_a, activity_b) {
  rule <- list()
  rule$activity_a <- activity_a
  rule$activity_b <- activity_b
  class(rule) <- c("conformance_rule", "list")
  attr(rule, "type") <- "respondend_existence"
  attr(rule, "checker") <- function(eventlog, rule) {
    eventlog %>%
      group_by_case() %>%
      mutate(rule_holds = (any(!!activity_id_(eventlog) == rule$activity_a) &
                             any(!!activity_id_(eventlog) == rule$activity_b)) |
               !any(!!activity_id_(eventlog) == rule$activity_a)) %>%
      ungroup_eventlog()
  }
  attr(rule, "label") <- paste0("responded_existence_",
                                str_replace(activity_a, "-| ", "_"),
                                "_",
                                str_replace(activity_b, "-| ", "_"))
  return(rule)
}
