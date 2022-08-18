
#' @importFrom rlang sym
#'
case_id_ <- function(eventlog) sym(case_id(eventlog))
activity_id_ <- function(eventlog) sym(activity_id(eventlog))
activity_instance_id_ <- function(eventlog) sym(activity_instance_id(eventlog))
resource_id_ <- function(eventlog) sym(resource_id(eventlog))
timestamp_ <- function(eventlog) sym(timestamp(eventlog))
lifecycle_id_ <- function(eventlog) sym(lifecycle_id(eventlog))

check_activity_in_log <- function(activity, log) {

  if(!(activity %in% activity_labels(log))) {
    stop(glue("Activity {activity} not found in log."))
  }
}

# Warning: The `eventlog` argument of `func()` is deprecated as of processcheckR 0.2.0.
# Please use the `log` argument instead.
# WARNING: Works only on exported functions!
lifecycle_warning_eventlog <- function (log, eventlog = deprecated()) {

  cl <- sys.call(-1L)
  func <- get(as.character(cl[[1L]]), mode = "function", envir = sys.frame(-2L))
  func_name <- match.call(definition = func, call = cl)[[1L]]

  if(lifecycle::is_present(eventlog)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = paste0(func_name, "(eventlog)"),
      with = paste0(func_name, "(log)"))
    return(eventlog)
  }

  return(log)
}