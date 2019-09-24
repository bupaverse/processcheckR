

#' Check declarative rules.
#'
#' This function can be used to check rules on event data. It needs an event log and a rule. Rules can be made with the following functions:
#' absent(),
#' and(),
#' contains(),
#' contains_between(),
#' contains_exactly(),
#' ends(),
#' precedence(),
#' response(),
#' responded_existence(),
#' starts(),
#' succession(),
#' xor().
#'
#' @return
#'
#' An annotated event log, where a new column indicates whether the rule holds or not. The name of the new column can optionally be set using the "label" argument.
#'
#'
#' @param eventlog Eventlog object
#' @param rule A rule create by a rule function.
#' @param label Optionally, the variable name under which the result of the rule should be stored.
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # check whether MRI Scan is preceded by Blood test.
#' patients %>%
#' check_rule(precedence("Blood test","MRI SCAN"))
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
    mutate(!!sym(label) := rule_holds) %>%
    select(-rule_holds)
}


