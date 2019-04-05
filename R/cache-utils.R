cleaned_namespaces_ <- function(
  default = storr::storr_environment()$default_namespace
) {
  out <- c(default, "meta")
  sort(out)
}

clear_tmp_namespace <- function(cache, jobs, namespace) {
  lightly_parallelize(
    X = cache$list(),
    FUN = function(target) {
      cache$del(key = target, namespace = namespace)
    },
    jobs = jobs
  )
  cache$clear(namespace = namespace)
  invisible()
}

default_cache_path <- function(root = getwd()) {
  file.path(root, ".drake")
}

# Generate a flat text log file to represent the state of the cache.
drake_cache_log_file_ <- function(
  file = "drake_cache.log",
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L,
  jobs = 1L,
  targets_only = FALSE
) {
  if (!length(file) || identical(file, FALSE)) {
    return(invisible())
  } else if (identical(file, TRUE)) {
    file <- formals(drake_cache_log_file_)$file
  }
  out <- drake_cache_log(
    path = path,
    search = search,
    cache = cache,
    verbose = verbose,
    jobs = jobs,
    targets_only = targets_only
  )
  # Suppress partial arg match warnings.
  suppressWarnings(
    write.table(
      x = out,
      file = file,
      quote = FALSE,
      row.names = FALSE
    )
  )
}

drake_try_fetch_rds <- function(path) {
  out <- try(drake_fetch_rds(path = path), silent = TRUE)
  if (!inherits(out, "try-error")) {
    return(out)
  }
  stop(
    "drake failed to get the storr::storr_rds() cache at ", path, ". ",
    "Something is wrong with the file system of the cache. ",
    "If you downloaded it from an online repository, are you sure ",
    "all the files were downloaded correctly? ",
    "If all else fails, remove the folder at ", path, " and try again.",
    call. = FALSE
  )
}

drake_fetch_rds <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  hash_algo_file <- file.path(path, "config", "hash_algorithm")
  hash_algo <- scan(hash_algo_file, quiet = TRUE, what = character())
  storr::storr_rds(path = path, hash_algorithm = hash_algo)
}

# Pre-set the values to avoid https://github.com/richfitz/storr/issues/80.
init_common_values <- function(cache) {
  common_values <- list(TRUE, FALSE, "done", "running", "failed")
  cache$mset(
    key = as.character(common_values),
    value = common_values,
    namespace = "common"
  )
}

is_imported_cache <- Vectorize(function(target, cache) {
  cache$exists(key = target) &&
    diagnose(
      target = target,
      character_only = TRUE,
      cache = cache
    )$imported
},
"target", SIMPLIFY = TRUE)

keys_are_mangled <- function(cache) {
  "driver_rds" %in% class(cache$driver) &&
    identical(cache$driver$mangle_key, TRUE)
}

list_multiple_namespaces <- function(cache, namespaces, jobs = 1) {
  out <- lightly_parallelize(
    X = namespaces,
    FUN = function(namespace) {
      cache$list(namespace = namespace)
    },
    jobs = jobs
  )
  Reduce(out, f = base::union)
}

memo_expr <- function(expr, cache, ...) {
  if (is.null(cache)) {
    return(force(expr))
  }
  lang <- match.call(expand.dots = FALSE)$expr
  key <- digest::digest(list(lang, ...), algo = cache$driver$hash_algorithm)
  if (cache$exists(key = key, namespace = "memoize")) {
    return(cache$get(key = key, namespace = "memoize"))
  }
  value <- force(expr)
  cache$set(key = key, value = value, namespace = "memoize")
  value
}

# Load an existing drake files system cache if it exists
# or create a new one otherwise.
recover_cache_ <- function(
  path = NULL,
  hash_algorithm = NULL,
  short_hash_algo = NULL,
  long_hash_algo = NULL,
  force = FALSE,
  verbose = 1L,
  fetch_cache = NULL,
  console_log_file = NULL
) {
  path <- path %||% default_cache_path()
  deprecate_force(force)
  deprecate_fetch_cache(fetch_cache)
  deprecate_hash_algo_args(short_hash_algo, long_hash_algo)
  hash_algorithm <- set_hash_algorithm(hash_algorithm)
  cache <- this_cache_(
    path = path,
    verbose = verbose,
    fetch_cache = fetch_cache,
    console_log_file = console_log_file
  )
  if (is.null(cache)) {
    cache <- new_cache(
      path = path,
      verbose = verbose,
      hash_algorithm = hash_algorithm,
      fetch_cache = fetch_cache,
      console_log_file = console_log_file
    )
  }
  init_common_values(cache)
  cache
}

safe_get <- function(key, namespace, config) {
  out <- just_try(config$cache$get(key = key, namespace = namespace))
  if (inherits(out, "try-error")) {
    out <- NA_character_
  }
  out
}

safe_get_hash <- function(key, namespace, config) {
  out <- just_try(config$cache$get_hash(key = key, namespace = namespace))
  if (inherits(out, "try-error")) {
    out <- NA_character_
  }
  out
}

set_hash_algorithm <- function(hash_algorithm) {
  if (is.null(hash_algorithm)) {
    "xxhash64"
  } else {
    hash_algorithm
  }
}

single_cache_log <- function(key, cache) {
  hash <- cache$get_hash(key = key)
  imported <- get_from_subspace(
    key = key,
    subspace = "imported",
    namespace = "meta",
    cache = cache
  )
  imported <- ifelse(is.na(imported), TRUE, imported)
  type <- ifelse(imported, "import", "target")
  weak_tibble(hash = hash, type = type, name = key)
}

target_exists <- function(target, config) {
  config$cache$exists(key = target)
}

targets_only <- function(targets, cache, jobs) {
  parallel_filter(
    x = targets,
    f = function(target) {
      !is_imported_cache(target = target, cache = cache) &&
        !is_encoded_path(target)
    },
    jobs = jobs
  )
}

# List the `storr` cache namespaces that store target-level information.
target_namespaces_ <- function(
  default = storr::storr_environment()$default_namespace
) {
  out <- c(
    cleaned_namespaces_(default = default),
    "progress"
  )
  sort(out)
}

this_cache_ <- function(
  path = NULL,
  force = FALSE,
  verbose = 1L,
  fetch_cache = NULL,
  console_log_file = NULL
) {
  path <- path %||% default_cache_path()
  deprecate_force(force)
  deprecate_fetch_cache(fetch_cache)
  usual_path_missing <- is.null(path) || !file.exists(path)
  if (usual_path_missing) {
    return(NULL)
  }
  config <- list(verbose = verbose, console_log_file = console_log_file)
  log_msg("cache", path, config = config)
  cache <- drake_try_fetch_rds(path = path)
  cache_vers_warn(cache = cache)
  cache
}
