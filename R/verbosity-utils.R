# Output a random tip about drake.
drake_tip_ <- function() {
  tips <- c(
    "A new and improved way to create large drake plans:
     https://ropenscilabs.github.io/drake-manual/plans.html#large-plans",

    "Use diagnose() to retrieve
     errors, warnings, messages, commands, runtimes, etc.",

    "Use drake_example() to download code for a small drake workflow.",

    "Check out the reference website https://ropensci.github.io/drake
     and user manual https://ropenscilabs.github.io/drake-manual.",

    "drake quickstart:
     load_mtcars_example();
     make(my_plan);
     readd(small)"
  )
  tips <- wrap_text(tips)
  sample(tips, 1)
}

drake_tip_message <- function() {
  packageStartupMessage(drake_tip_())
}

crop_lines <- function(x, n = 10) {
  if (length(x) > n) {
    x <- x[1:(n - 1)]
    x[n] <- "..."
  }
  x
}

crop_text <- Vectorize(function(x, width = getOption("width")) {
  if (nchar(x) > width) {
    x <- paste0(substr(x, 1, width - 3), "...")
  }
  x
},
"x", USE.NAMES = FALSE)

drake_error <- function(..., config) {
  log_to_file(paste("Error:", ...), config = config)
  stop(..., call. = FALSE)
}

drake_warning <- function(..., config) {
  log_to_file(paste("Warning:", ...), config = config)
  warning(..., call. = FALSE)
}

log_msg <- function(..., config, tier = 2L, color = colors["default"]) {
  log_to_file(..., config = config)
  if (is.null(config$verbose) || as.integer(config$verbose) < tier) {
    return()
  }
  if (tier > 1L) {
    if (!is.null(.pkg_envir$spinner)) {
      .pkg_envir$spinner$spin()
    }
    return()
  }
  msg <- c(...)
  if (!is.null(color) && requireNamespace("crayon", quietly = TRUE)) {
    msg[1] <- crayon::make_style(color)(msg[1])
  }
  if (config$verbose > 1L) {
    msg[1] <- paste0("\r", msg[1])
  }
  message(crop_text(paste(msg, collapse = " ")))
}

log_to_file <- function(..., config) {
  if (is.null(config$console_log_file)) {
    return()
  }
  write(
    x = paste(microtimestamp(), "|", ...),
    file = config$console_log_file,
    append = TRUE
  )
  invisible()
}

msg_time <- function(target, meta, config) {
  if (requireNamespace("lubridate", quietly = TRUE)) {
    exec <- round(lubridate::dseconds(meta$time_command$elapsed), 3)
    total <- round(lubridate::dseconds( meta$time_build$elapsed), 3)
    tail <- paste("", exec, "|", total, " (exec | total)")
  } else {
    tail <- " (install lubridate)" # nocov
  }
  log_msg("time ", target, tail = tail, config = config)
}

multiline_message <- function(x) {
  n <- 30
  if (length(x) > n) {
    x <- c(x[1:(n - 1)], "...")
  }
  x <- paste0("  ", x)
  paste(x, collapse = "\n")
}

wrap_text <- Vectorize(
  function(x) {
    x <- paste(strwrap(x), collapse = "\n")
    unname(x)
  },
  "x"
)

