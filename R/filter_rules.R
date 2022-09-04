#' @title Filter Using Declarative Rules
#'
#' @description This function can be used to filter event data using declaritive rules. It needs a `log` (object of class
#' \code{\link[bupaR]{log}} or derivatives, e.g. \code{\link[bupaR]{grouped_log}}, \code{\link[bupaR]{eventlog}},
#' \code{\link[bupaR]{activitylog}}, etc.). and a set of `rules`.
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
#' A filtered log (of same type as input) that satisfied the specified rules.
#'
#' @inherit check_rules params
#'
#' @seealso \code{\link{check_rules}}
#'
#' @examples
#' library(bupaR)
#' library(eventdataR)
#'
#' # Filter where Blood test precedes MRI SCAN and Registration is the start of the case.
#' patients %>%
#'  filter_rules(precedence("Blood test","MRI SCAN"),
#'               starts("Registration"))
#'
#' @export filter_rules
filter_rules <- function(log, ..., eventlog = deprecated()) {
  UseMethod("filter_rules")
}

#' @describeIn filter_rules Filter a \code{\link[bupaR]{log}} using declaritive rules.
#' @export
filter_rules.log <- function(log, ..., eventlog = deprecated()) {

  log <- lifecycle_warning_eventlog(log, eventlog)

  .args <- list(...)

  if(length(.args) < 1) {
    stop("At least one filtering rules should be supplied.")
  }

  #.args <- as.list(match.call(expand.dots = TRUE)[-1:-2])

  #names(rules)[3:length(rules)] <- paste0("filtering_rule", seq(from = 1, to = length(rules) - 2, by = 1))
  #names(.args) <- paste0("filtering_rule", seq(from = 1, to = length(.args), by = 1))
  names(.args) <- paste0("filtering_rule", seq_along(.args))

  log <- do.call("check_rules_internal", list(log, .args))

  log %>%
    filter_at(vars(starts_with("filtering_rule")), all_vars(. == TRUE)) %>%
    select_at(vars(-starts_with("filtering_rule")))
}
