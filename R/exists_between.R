
#' Check if activity is present (exists) in a case between  min and max number of times
#'
#' @param activity Activity too check
#' @param min The minimum number of times the activity should be present.
#' @param max The maximum number of times the activity should be present.
#'
#' @export
#'
exists_between <- function(activity, min = 1, max = 1) {
  if(max < min) {
    stop("max should be greater or equal than min")
  }
  rule <- list()
  rule$activity <- activity
  rule$min <- min
  rule$max <- max
  class(rule) <- c("conformance_rule","list")
  attr(rule, "type") <- "exists_between"
  attr(rule, "checker") <- function(eventlog, rule) {
    eventlog %>%
      filter_activity(activities = rule$activity) %>%
      group_by(!!case_id_(eventlog)) %>%
      n_activity_instances() %>%
      filter(between(n_activity_instances, rule$min, rule$max)) %>%
      pull(1) -> holds

    eventlog %>%
      mutate(rule_holds = !!case_id_(eventlog) %in% holds) %>%
      return()
  }
  attr(rule, "label") <- paste0("exists_between_", str_replace(activity,"-| ", "_"), "_", min,"_",max)
  return(rule)
}
