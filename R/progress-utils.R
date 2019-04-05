get_progress_single <- function(target, cache) {
  if (cache$exists(key = target, namespace = "progress")) {
    cache$get(key = target, namespace = "progress")
  } else{
    "none"
  }
}

set_progress <- function(target, meta, value, config) {
  if (!config$log_progress || (meta$imported %||% FALSE)) {
    return()
  }
  config$cache$duplicate(
    key_src = value,
    key_dest = target,
    namespace_src = "common",
    namespace_dest = "progress"
  )
}
