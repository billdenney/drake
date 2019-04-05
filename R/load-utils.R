bind_load_target <- function(target, cache, namespace, envir, verbose) {
  assert_pkg("bindr")
  # Allow active bindings to overwrite existing variables.
  if (exists(x = target, envir = envir, inherits = FALSE)) {
    message(
      "Replacing already-loaded variable ", target,
      " with an active binding."
    )
    remove(list = target, envir = envir)
  }
  bindr::populate_env(
    env = envir,
    names = as.character(target),
    fun = function(key, cache, namespace) {
      if (!length(namespace)) {
        # Now impractical to cover because loadd() checks the namespace,
        # but good to have around anyway.
        namespace <- cache$default_namespace # nocov
      }
      cache$get(
        key = as.character(key),
        namespace = as.character(namespace),
        use_cache = TRUE
      )
    },
    cache = cache,
    namespace = namespace
  )
}

#' @title Load a target right away (internal function)
#' @description This function is only exported
#' to make active bindings work safely.
#' It is not actually a user-side function.
#' @keywords internal
#' @export
#' @inheritParams loadd
eager_load_target <- function(target, cache, namespace, envir, verbose) {
  value <- cache$get(key = target, namespace = namespace)
  assign(x = target, value = value, envir = envir)
  local <- environment()
  rm(value, envir = local)
  invisible()
}

load_target <- function(target, cache, namespace, envir, verbose, lazy) {
  switch(
    lazy,
    eager = eager_load_target(
      target = target,
      cache = cache,
      namespace = namespace,
      envir = envir,
      verbose = verbose
    ),
    promise = promise_load_target(
      target = target,
      cache = cache,
      namespace = namespace,
      envir = envir,
      verbose = verbose
    ),
    bind = bind_load_target(
      target = target,
      cache = cache,
      namespace = namespace,
      envir = envir,
      verbose = verbose
    )
  )
}

parse_lazy_arg <- function(lazy) {
  if (identical(lazy, FALSE)) {
    "eager"
  } else if (identical(lazy, TRUE)) {
    "promise"
  } else {
    match.arg(arg = lazy, choices = c("eager", "promise", "bind"))
  }
}

promise_load_target <- function(target, cache, namespace, envir, verbose) {
  eval_env <- environment()
  delayedAssign(
    x = target,
    value = cache$get(key = target, namespace = namespace),
    eval.env = eval_env,
    assign.env = envir
  )
}
