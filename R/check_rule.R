

#' Check declarative rules.
#'
#' @param eventlog Eventlog object
#' @param rule A rule create by a rule function.
#' @param label Optionally, the variable name under which the result of the rule should be stored.
#'
#' @export
#'
check_rule <- function(eventlog, rule, label = NULL) {
  rule_holds <- NULL
  if(is.null(label)) {
    label <- attr(rule, "label")
  }
  eventlog %>%
    attr(rule, "checker")(rule) %>%
    rename(!!sym(label) := rule_holds)
}


