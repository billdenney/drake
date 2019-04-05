abort_full_clean <- function() {
  menu_enabled <- .pkg_envir[["drake_clean_menu"]] %||%
    getOption("drake_clean_menu") %||%
    TRUE
  if (!(interactive() && menu_enabled)) {
    return(FALSE)
  }
  # nocov start
  title <- paste(
    "Really delete everything in the drake cache?",
    "(Prompt shown once per session.)",
    sep = "\n"
  )
  response <- utils::menu(choices = c("yes", "no"), title = title)
  .pkg_envir[["drake_clean_menu"]] <- FALSE
  !identical(as.integer(response), 1L)
  # nocov end
}

clean_single_target <- function(
  target,
  cache,
  namespaces,
  graph,
  layout
) {
  files <- character(0)
  if (cache$exists(target, namespace = "meta")) {
    files <- cache$get(key = target, namespace = "meta")$file_out
  }
  for (namespace in namespaces) {
    for (key in c(target, files)) {
      try(cache$del(key = key, namespace = namespace))
    }
  }
  if (length(files)) {
    unlink(decode_path(files), recursive = TRUE)
  }
}

rescue_del <- function(key, cache, namespace) {
  tryCatch(
    touch_storr_object(key = key, cache = cache, namespace = namespace),
    error = function(e) {
      cache$del(key = key, namespace = namespace)
    }
  )
  invisible(NULL)
}

touch_storr_object <- function(key, cache, namespace) {
  envir <- environment()
  hash <- cache$get_hash(key = key, namespace = namespace)
  value <- cache$driver$get_object(hash = hash)
  remove(value, envir = envir)
  invisible(NULL)
}
