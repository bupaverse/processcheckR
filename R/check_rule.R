#' @title Check Declarative Rule(s)
#'
#' @description This function can be used to check rules or constraint templates on event data.
#' It needs a `log` (object of class \code{\link[bupaR]{log}} or derivatives, e.g. \code{\link[bupaR]{grouped_log}},
#' \code{\link[bupaR]{eventlog}}, \code{\link[bupaR]{activitylog}}, etc.). and (a) `rule`(s).
#' Rules can be made with the following templates:
#' \itemize{
#'  \item \emph{Cardinality}:
#'  \itemize{
#'      \item \code{\link{absent}}: Check if the specified activity is absent from a case,
#'      \item \code{\link{contains}}: Check if the specified activity is present (contained) in a case,
#'      \item \code{\link{contains_between}}: Check if the specified activity is present (contained) in a case between the minimum and maximum number of times,
#'      \item \code{\link{contains_exactly}}: Check if the specified activity is present (contained) in a case for exactly `n` times.
#'  }
#'  \item \emph{Relation}:
#'  \itemize{
#'      \item \code{\link{ends}}: Check if cases end with the specified activity,
#'      \item \code{\link{starts}}: Check if cases start with the specified activity.
#'      \item \code{\link{precedence}}: Check for precedence between two activities,
#'      \item \code{\link{response}}: Check for response between two activities,
#'      \item \code{\link{responded_existence}}: Check for responded existence between two activities,
#'      \item \code{\link{succession}}: Check for succession between two activities.
#'  }
#'  \item \emph{Exclusiveness}:
#'  \itemize{
#'      \item \code{\link{and}}: Check for co-existence of two activities,
#'      \item \code{\link{xor}}: Check for exclusiveness of two activities.
#'  }
#' }
#'
#' @details
#' The rules or constraint templates in this package are (partially) based on \emph{DecSerFlow} (\emph{Declarative Service Flow Language}).
#' For more information, see the \strong{References} below.
#'
#' ## Grouped Logs
#' When applied to a \code{\link[bupaR]{grouped_log}}, the grouping variables are ignored but retained in the returned log.
#'
#' @return
#' An annotated log (of same type as input), where – for every rule – a new column indicates whether the rule holds or not.
#' The name of the new column can optionally be set using the `label` argument, or by the name of each rule in the name-rule pairs.
#'
#' @param log \code{\link[bupaR]{log}}: Object of class \code{\link[bupaR]{log}} or derivatives (\code{\link[bupaR]{grouped_log}}, \code{\link[bupaR]{eventlog}}, \code{\link[bupaR]{activitylog}}, etc.).
#' @param rule A rule created by a rule function.
#' @param label \code{\link{character}} (default \code{\link{NULL}}): Optionally, the column name under which the result of the rule should be stored.
#' @param ... Name-rule pairs created by rule functions.
#' @param eventlog `r lifecycle::badge("deprecated")`; please use \code{log} instead.
#'
#' @seealso \code{\link{filter_rules}}
#'
#' @references van der Aalst, W. M. P., & Pesic, M. (2006). DecSerFlow: Towards a Truly Declarative Service Flow Language.
#' In M. Bravetti, M. Núñez, & G. Zavattaro (Eds.), Proceedings of the 3rd International Workshop on Web Services and Formal Methods (Vol. 4184, pp. 1–23).
#' Springer. \url{https://doi.org/10.1007/11841197_1}
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # Check whether MRI Scan is preceded by Blood test.
#' patients %>%
#'  check_rule(precedence("Blood test","MRI SCAN"))
#'
#' # Check whether MRI Scan is preceded by Blood test, and the case starts with Registration.
#' patients %>%
#'  check_rules(rule1 = precedence("Blood test","MRI SCAN"),
#'              rule2 = starts("Registration"))
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
  } else if (is.activitylog(log) & label %in% bupaR:::allowed_lifecycles) {
    abort(glue("Label \"{label}\" is a reserved column name for activitylog timestamps ({stringi::stri_flatten(dQuote(bupaR:::allowed_lifecycles, q = FALSE), collapse = ',')}).
    Please use another label to prevent unintended consequences."))
  } else if (label %in% colnames(log)) {
    warn(glue("Column names already contain label: \"{label}\". Column will be overwritten. This can have unintended consequences."))
  }

  log %>%
    attr(rule, "checker")(rule) %>%
    mutate(!!sym(label) := rule_holds) %>%
    select(-rule_holds)
}