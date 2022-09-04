#' @title Check Declarative Rule
#'
#' @description This function can be used to check rules on event data. It needs a `log` (object of class \code{\link[bupaR]{log}} or derivatives,
#' e.g. \code{\link[bupaR]{grouped_log}}, \code{\link[bupaR]{eventlog}}, \code{\link[bupaR]{activitylog}}, etc.). and a `rule`.
#' Rules can be made with the following functions:
#' \itemize{
#' \item \code{\link{absent}},
#' \item \code{\link{and}},
#' \item \code{\link{contains}},
#' \item \code{\link{contains_between}},
#' \item \code{\link{contains_exactly}},
#' \item \code{\link{ends}},
#' \item \code{\link{precedence}},
#' \item \code{\link{response}},
#' \item \code{\link{responded_existence}},
#' \item \code{\link{starts}},
#' \item \code{\link{succession}},
#' \item \code{\link{xor}}.
#' }
#'
#' @return
#' An annotated log (of same type as input), where a new column indicates whether the rule holds or not.
#' The name of the new column can optionally be set using the `label` argument.
#'
#' @param log \code{\link[bupaR]{log}}: Object of class \code{\link[bupaR]{log}} or derivatives (\code{\link[bupaR]{grouped_log}}, \code{\link[bupaR]{eventlog}}, \code{\link[bupaR]{activitylog}}, etc.).
#' @param rule A rule created by a rule function.
#' @param label \code{\link{character}} (default \code{\link{NULL}}): Optionally, the column name under which the result of the rule should be stored.
#' @param eventlog `r lifecycle::badge("deprecated")`; please use \code{log} instead.
#'
#' @seealso \code{\link{check_rules}}
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # check whether MRI Scan is preceded by Blood test.
#' patients %>%
#'  check_rule(precedence("Blood test","MRI SCAN"))
#'
#' @export check_rule
check_rule <- function(log, rule, label = NULL, eventlog = deprecated()) {
  UseMethod("check_rule")
}

#' @describeIn check_rule Check rule on a \code{\link[bupaR]{log}}.
#' @export
check_rule.log <- function(log, rule, label = NULL, eventlog = deprecated()) {

  log <- lifecycle_warning_eventlog(log, eventlog)

  rule_holds <- NULL

  if(is.null(label)) {
    label <- attr(rule, "label")
  }

  log %>%
    attr(rule, "checker")(rule) %>%
    mutate(!!sym(label) := rule_holds) %>%
    select(-rule_holds)
}


