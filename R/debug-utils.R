debug_command <- function(command) {
  if (is.character(command)) {
    debug_command_char(command)
  } else {
    . <- NULL
    out <- safe_deparse(command)
    out <- debug_command_char(out)
    parse(text = out)[[1]]
  }
}

debug_command_char <- function(command) {
  paste0("drake::debug_and_run(function() {\n", command, "\n})")
}

#' @title Run a function in debug mode.
#' @description Internal function for [drake_debug()]. Not for general use.
#' @keywords internal
#' @seealso [drake_debug()]
#' @export
#' @return The return value of `f`.
#' @param f A function.
debug_and_run <- function(f) {
  # Tested in tests/testthat/test-always-skipped.R.
  # nocov start
  debug(f)
  f()
  # nocov end
}
