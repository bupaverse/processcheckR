
#' @export check_rules
#' @rdname check_rule
check_rules <- function(log, ..., eventlog = deprecated()) {
  UseMethod("check_rules")
}

#' @describeIn check_rule Check rules on a \code{\link[bupaR]{log}}.
#' @export
check_rules.log <- function(log, ..., eventlog = deprecated()) {

  log <- lifecycle_warning_eventlog(log, eventlog)

  rules <- list(...)

  #if(any(duplicated(names(rules)))) {
  #  warning("Some rules have duplicate labels and will be overwritten.")
  #}

  #for(i in seq_along(rules)) {
  #   log <- check_rule(log, rules[[i]], label = names(rules)[i])
  #}

  check_rules_internal(log, rules)
}

check_rules_internal <- function(log, rules) {

  if(any(duplicated(names(rules)))) {
    warn("Some rules have duplicate labels and will be overwritten.")
  }

  for(i in seq_along(rules)) {
    log <- check_rule(log, rules[[i]], label = names(rules)[i])
  }

  return(log)
}
