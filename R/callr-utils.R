default_drake_source <- "_drake.R"

r_assert_source <- function(source) {
  if (file.exists(source)) {
    return()
  }
  stop(
    "File ", shQuote(source), " does not exist.\n",
    "Functions r_make() and friends need an R script file\n",
    "to configure your drake project.\n",
    "The configuration R script can be\n",
    "  1. The file you supply to the ", shQuote(source), " argument, or\n",
    "  2. The file you supply via options(drake_source = \"file.R\"), or\n",
    "  3. A file called ", shQuote("_drake.R"), " (default).\n",
    "Read more: \n",
    "https://ropenscilabs.github.io/drake-manual/projects.html#safer-interactivity", # nolint
    call. = FALSE
  )
}

r_drake <- function(source, d_fn, d_args, r_fn, r_args) {
  assert_pkg("callr")
  r_args$func <- function(source, d_fn, d_args) {
    d_args$config <- source(source)$value
    do.call(d_fn, d_args)
  }
  source <- source %||% getOption("drake_source") %||% default_drake_source
  r_assert_source(source)
  r_args$args <- list(source = source, d_fn = d_fn, d_args = d_args)
  r_fn <- r_fn %||% callr::r
  if ("show" %in% names(formals(r_fn))) {
    r_args$show <- r_args$show %||% TRUE
  }
  do.call(r_fn, r_args)
}
