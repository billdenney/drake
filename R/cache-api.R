#' @title List targets in the cache.
#' @description Tip: read/load a cached item with [readd()]
#'   or [loadd()].
#' @seealso [readd()], [loadd()],
#'   [drake_plan()], [make()]
#' @export
#' @return Either a named logical indicating whether the given
#'   targets or cached or a character vector listing all cached
#'   items, depending on whether any targets are specified.
#'
#' @inheritParams drake_config
#'
#' @param ... Deprecated. Do not use.
#'   Objects to load from the cache, as names (unquoted)
#'   or character strings (quoted). Similar to `...` in
#'   `remove()`.
#'
#' @param list Deprecated. Do not use.
#'   Character vector naming objects to be loaded from the
#'   cache. Similar to the `list` argument of [remove()].
#'
#' @param targets_only Logical. If `TRUE` just list the targets.
#'   If `FALSE`, list files and imported objects too.
#'
#' @param no_imported_objects Logical, deprecated. Use
#'   `targets_only` instead.
#'
#' @param cache drake cache. See [new_cache()].
#'   If supplied, `path` and `search` are ignored.
#'
#' @param path Root directory of the drake project,
#'   or if `search` is `TRUE`, either the
#'   project root or a subdirectory of the project.
#'   Ignored if a `cache` is supplied.
#'
#' @param search Logical. If `TRUE`, search parent directories
#'   to find the nearest drake cache. Otherwise, look in the
#'   current working directory only.
#'   Ignored if a `cache` is supplied.
#'
#' @param namespace Character scalar, name of the storr namespace
#'   to use for listing objects.
#'
#' @param jobs Number of jobs/workers for parallel processing.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' if (requireNamespace("lubridate")) {
#' load_mtcars_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build all the targets.
#' cached()
#' cached(targets_only = FALSE)
#' }
#' }
#' })
#' }
cached <- function(
  ...,
  list = character(0),
  no_imported_objects = FALSE,
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = 1L,
  namespace = NULL,
  jobs = 1,
  targets_only = TRUE
) {
  if (is.null(cache)) {
    cache <- get_cache(path = path, search = search, verbose = verbose)
  }
  if (is.null(cache)) {
    return(character(0))
  }
  if (is.null(namespace)) {
    namespace <- cache$default_namespace
  }
  targets <- c(list, match.call(expand.dots = FALSE)$...)
  if (length(targets)) {
    warning(
      "The `...` and `list` arguments of `cached()` are deprecated.",
      "`cached()` no longer accepts target names. It just lists ",
      "the targets in the cache.",
      call. = FALSE
    )
  }
  targets <- cache$list(namespace = namespace)
  if (targets_only) {
    targets <- targets_only(targets, cache, jobs)
  }
  display_keys(targets)
}

#' @title Get a table that represents the state of the cache.
#' @description
#' This functionality is like
#' `make(..., cache_log_file = TRUE)`,
#' but separated and more customizable. Hopefully, this functionality
#' is a step toward better data versioning tools.
#' @details A hash is a fingerprint of an object's value.
#' Together, the hash keys of all your targets and imports
#' represent the state of your project.
#' Use `drake_cache_log()` to generate a data frame
#' with the hash keys of all the targets and imports
#' stored in your cache.
#' This function is particularly useful if you are
#' storing your drake project in a version control repository.
#' The cache has a lot of tiny files, so you should not put it
#' under version control. Instead, save the output
#' of `drake_cache_log()` as a text file after each [make()],
#' and put the text file under version control.
#' That way, you have a changelog of your project's results.
#' See the examples below for details.
#' Depending on your project's
#' history, the targets may be different than the ones
#' in your workflow plan data frame.
#' Also, the keys depend on the hash algorithm
#' of your cache. To define your own hash algorithm,
#' you can create your own `storr` cache and give it a hash algorithm
#' (e.g. `storr_rds(hash_algorithm = "murmur32")`)
#' @seealso [cached()], [get_cache()]
#' @export
#' @return Data frame of the hash keys of the targets and imports
#'   in the cache
#'
#' @inheritParams cached
#'
#' @param jobs Number of jobs/workers for parallel processing.
#'
#' @param targets_only Logical, whether to output information
#'   only on the targets in your workflow plan data frame.
#'   If `targets_only` is `FALSE`, the output will
#'   include the hashes of both targets and imports.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' # Load drake's canonical example.
#' load_mtcars_example() # Get the code with drake_example()
#' # Run the project, build all the targets.
#' make(my_plan)
#' # Get a data frame of all the hash keys.
#' # If you want a changelog, be sure to do this after every make().
#' cache_log <- drake_cache_log()
#' head(cache_log)
#' # Suppress partial arg match warnings.
#' suppressWarnings(
#'   # Save the hash log as a flat text file.
#'   write.table(
#'     x = cache_log,
#'     file = "drake_cache.log",
#'     quote = FALSE,
#'     row.names = FALSE
#'   )
#' )
#' # At this point, put drake_cache.log under version control
#' # (e.g. with 'git add drake_cache.log') alongside your code.
#' # Now, every time you run your project, your commit history
#' # of hash_lot.txt is a changelog of the project's results.
#' # It shows which targets and imports changed on every commit.
#' # It is extremely difficult to track your results this way
#' # by putting the raw '.drake/' cache itself under version control.
#' }
#' })
#' }
drake_cache_log <- function(
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L,
  jobs = 1,
  targets_only = FALSE
) {
  if (is.null(cache)) {
    return(
      weak_tibble(
        hash = character(0),
        type = character(0),
        name = character(0)
      )
    )
  }
  out <- lightly_parallelize(
    X = cache$list(),
    FUN = single_cache_log,
    jobs = jobs,
    cache = cache
  )
  out <- weak_as_tibble(do.call(rbind, out))
  if (targets_only) {
    out <- out[out$type == "target", ]
  }
  out$name <- display_keys(out$name)
  out
}

#' @title Search up the file system for the nearest drake cache.
#' @description Only works if the cache is a file system in a
#' hidden folder named `.drake` (default).
#' @seealso [drake_plan()], [make()],
#' @export
#' @return File path of the nearest drake cache or `NULL`
#'   if no cache is found.
#' @param path Starting path for search back for the cache.
#'   Should be a subdirectory of the drake project.
#' @param dir Character, name of the folder containing the cache.
#' @param directory Deprecated. Use `dir`.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the target.
#' # Find the file path of the project's cache.
#' # Search up through parent directories if necessary.
#' find_cache()
#' }
#' })
#' }
find_cache <- function(
  path = getwd(),
  dir = NULL,
  directory = NULL
) {
  if (!is.null(directory)) {
    warning(
      "Argument `directory` of find_cache() is deprecated. ",
      "use `dir` instead.",
      call. = FALSE
    )
  }
  dir <- dir %||% basename(default_cache_path())
  while (!(dir %in% list.files(path = path, all.files = TRUE))) {
    path <- dirname(path)
    # If we can search no higher...
    if (path == dirname(path)) {
      return(NULL) # The cache does not exist
    }
  }
  file.path(path, dir)
}

#' @title Get the default cache of a `drake` project.
#' @description Only works if the cache
#' is in a folder called `.drake/`. See the description of the
#' `path` argument for details.
#' @seealso [new_cache()], [drake_config()]
#' @export
#' @return A drake/storr cache in a folder called `.drake/`,
#'   if available. `NULL` otherwise.
#' @inheritParams cached
#' @inheritParams drake_config
#' @param path Character, either the root file path of a `drake` project
#'   or a folder containing the root (top-level working directory
#'   where you plan to call [make()]).
#'   If this is too confusing, feel free to just use `storr::storr_rds()`
#'   to get the cache.
#'   If `search = FALSE`, `path` must be the root.
#'   If `search = TRUE`, you can specify any
#'   subdirectory of the project. Let's say `"/home/you/my_project"`
#'   is the root. The following are equivalent and correct:
#'   - `get_cache(path = "/home/you/my_project", search = FALSE)`
#'   - `get_cache(path = "/home/you/my_project", search = TRUE)`
#'   - `get_cache(path = "/home/you/my_project/subdir/x", search = TRUE)`
#'   - `get_cache(path = "/home/you/my_project/.drake", search = TRUE)`
#'   - `get_cache(path = "/home/you/my_project/.drake/keys", search = TRUE)`
#' @param force Deprecated.
#' @param fetch_cache Character vector containing lines of code.
#'   The purpose of this code is to fetch the `storr` cache
#'   with a command like `storr_rds()` or `storr_dbi()`,
#'   but customized. This feature is experimental.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' clean(destroy = TRUE)
#' # No cache is available.
#' get_cache() # NULL
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' x <- get_cache() # Now, there is a cache.
#' y <- storr::storr_rds(".drake") # Equivalent.
#' # List the objects readable from the cache with readd().
#' x$list()
#' }
#' })
#' }
get_cache <- function(
  path = getwd(),
  search = TRUE,
  verbose = 1L,
  force = FALSE,
  fetch_cache = NULL,
  console_log_file = NULL
) {
  deprecate_force(force)
  deprecate_fetch_cache(fetch_cache)
  if (search) {
    path <- find_cache(path = path)
  } else {
    path <- default_cache_path(root = path)
  }
  this_cache_(
    path = path,
    verbose = verbose,
    fetch_cache = fetch_cache,
    console_log_file = console_log_file
  )
}

#' @title Load one or more targets or imports from the drake cache.
#' @rdname readd
#' @seealso [cached()], [drake_plan()], [make()]
#' @export
#'
#' @inheritParams cached
#' @inheritParams readd
#'
#' @param ... Targets to load from the cache: as names (symbols) or
#'   character strings. If the `tidyselect` package is installed,
#'   you can also supply `dplyr`-style `tidyselect`
#'   commands such as `starts_with()`, `ends_with()`, and `one_of()`.
#'
#' @param list Character vector naming targets to be loaded from the
#'   cache. Similar to the `list` argument of [remove()].
#'
#' @param imported_only Logical, deprecated.
#'
#' @param envir Environment to load objects into. Defaults to the
#'   calling environment (current workspace).
#'
#' @param jobs Number of parallel jobs for loading objects. On
#'   non-Windows systems, the loading process for multiple objects
#'   can be lightly parallelized via `parallel::mclapply()`.
#'   just set jobs to be an integer greater than 1. On Windows,
#'   `jobs` is automatically demoted to 1.
#'
#' @param deps Logical, whether to load any cached
#'   dependencies of the targets
#'   instead of the targets themselves.
#'   This is useful if you know your
#'   target failed and you want to debug the command in an interactive
#'   session with the dependencies in your workspace.
#'   One caveat: to find the dependencies,
#'   [loadd()] uses information that was stored
#'   in a [drake_config()] list and cached
#'   during the last [make()].
#'   That means you need to have already called [make()]
#'   if you set `deps` to `TRUE`.
#'
#' @param lazy Either a string or a logical. Choices:
#'   - `"eager"`: no lazy loading. The target is loaded right away
#'     with [assign()].
#'   - `"promise"`: lazy loading with [delayedAssign()]
#'   - `"bind"`: lazy loading with active bindings:
#'     `bindr::populate_env()`.
#'   - `TRUE`: same as `"promise"`.
#'   - `FALSE`: same as `"eager"`.
#'
#' @param graph Deprecated.
#'
#' @param replace Logical. If `FALSE`,
#'   items already in your environment
#'   will not be replaced.
#'
#' @param tidyselect Logical, whether to enable
#'   `tidyselect` expressions in `...` like
#'   `starts_with("prefix")` and `ends_with("suffix")`.
#'
#' @param config Optional [drake_config()] object.
#'   You should supply one if `deps` is `TRUE`.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the projects, build the targets.
#' config <- drake_config(my_plan)
#' loadd(small) # Load target 'small' into your workspace.
#' small
#' # For many targets, you can parallelize loadd()
#' # using the 'jobs' argument.
#' loadd(list = c("small", "large"), jobs = 2)
#' ls()
#' # Load the dependencies of the target, coef_regression2_small
#' loadd(coef_regression2_small, deps = TRUE, config = config)
#' ls()
#' # Load all the targets listed in the workflow plan
#' # of the previous `make()`.
#' # If you do not supply any target names, `loadd()` loads all the targets.
#' # Be sure your computer has enough memory.
#' loadd()
#' ls()
#' }
#' })
#' }
loadd <- function(
  ...,
  list = character(0),
  imported_only = NULL,
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  namespace = NULL,
  envir = parent.frame(),
  jobs = 1,
  verbose = 1L,
  deps = FALSE,
  lazy = "eager",
  graph = NULL,
  replace = TRUE,
  show_source = FALSE,
  tidyselect = TRUE,
  config = NULL
) {
  force(envir)
  lazy <- parse_lazy_arg(lazy)
  if (!is.null(graph)) {
    warning(
      "argument `graph` is deprecated.",
      call. = FALSE
    ) # 2019-01-04 # nolint
  }
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  if (is.null(namespace)) {
    namespace <- cache$default_namespace
  }
  if (tidyselect && requireNamespace("tidyselect", quietly = TRUE)) {
    targets <- drake_tidyselect_cache(
      ...,
      list = list,
      cache = cache,
      namespaces = namespace
    )
  } else {
    targets <- c(as.character(match.call(expand.dots = FALSE)$...), list)
  }
  if (!length(targets) && !length(list(...))) {
    targets <- cache$list()
  }
  if (!is.null(imported_only)) {
    warning(
      "The `imported_only` argument of `loadd()` is deprecated. ",
      "In drake >= 7.0.0, loadd() only loads targets listed in the plan. ",
      "Do not give the names of imports or files ",
      "except to list them as dependencies ",
      "from within an R Markdown/knitr report. ",
      "Otherwise, imports and files are deliberately ignored.",
      call. = FALSE
    )
  }
  if (deps) {
    if (is.null(config)) {
      stop(
        "In `loadd(deps = TRUE)`, you must supply a `drake_config()` ",
        "object to the `config` argument.",
        call. = FALSE
      )
    }
    assert_config_not_plan(config)
    targets <- deps_memory(targets = targets, config = config)
  }
  exists <- lightly_parallelize(
    X = targets,
    FUN = cache$exists,
    jobs = jobs
  )
  exists <- unlist(exists)
  targets <- targets[exists]
  targets <- targets_only(targets, cache = cache, jobs = jobs)
  if (!length(targets) && !deps) {
    if (verbose) {
      message("No targets to load in loadd().")
    }
    stop(return(invisible))
  }
  if (!replace) {
    targets <- setdiff(targets, names(envir))
  }
  if (show_source) {
    lapply(
      X = targets,
      FUN = show_source,
      config = list(cache = cache),
      character_only = TRUE
    )
  }
  lightly_parallelize(
    X = targets, FUN = load_target, cache = cache,
    namespace = namespace, envir = envir,
    verbose = verbose, lazy = lazy
  )
  invisible()
}

#' @title  Make a new `drake` cache.
#' @description Uses the [storr_rds()] function
#' from the `storr` package.
#' @export
#' @return A newly created drake cache as a storr object.
#' @inheritParams cached
#' @inheritParams drake_config
#' @seealso [make()]
#' @param path File path to the cache if the cache
#'   is a file system cache.
#' @param type Deprecated argument. Once stood for cache type.
#'   Use `storr` to customize your caches instead.
#' @param hash_algorithm Name of a hash algorithm to use.
#'   See the `algo` argument of the `digest` package for your options.
#' @param short_hash_algo Deprecated on 2018-12-12. Use `hash_algorithm` instead.
#' @param long_hash_algo Deprecated on 2018-12-12. Use `hash_algorithm` instead.
#' @param ... other arguments to the cache constructor.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine new_cache() side effects.", {
#' clean(destroy = TRUE) # Should not be necessary.
#' unlink("not_hidden", recursive = TRUE) # Should not be necessary.
#' cache1 <- new_cache() # Creates a new hidden '.drake' folder.
#' cache2 <- new_cache(path = "not_hidden", hash_algorithm = "md5")
#' clean(destroy = TRUE, cache = cache2)
#' })
#' }
new_cache <- function(
  path = NULL,
  verbose = 1L,
  type = NULL,
  hash_algorithm = NULL,
  short_hash_algo = NULL,
  long_hash_algo = NULL,
  ...,
  console_log_file = NULL
) {
  path <- path %||% default_cache_path()
  hash_algorithm <- set_hash_algorithm(hash_algorithm)
  if (!is.null(type)) {
    warning(
      "The 'type' argument of new_cache() is deprecated. ",
      "Please see the storage guide in the manual for the new cache API:",
      "https://ropenscilabs.github.io/drake-manual/store.html"
    )
  }
  deprecate_hash_algo_args(short_hash_algo, long_hash_algo)
  config <- list(verbose = verbose, console_log_file = console_log_file)
  log_msg("cache", path, config = config)
  cache <- storr::storr_rds(
    path = path,
    mangle_key = FALSE,
    hash_algorithm = hash_algorithm
  )
  writeLines(
    text = c("*", "!/.gitignore"),
    con = file.path(path, ".gitignore")
  )
  cache
}

#' @title Read the pseudo-random number generator seed of the project.
#' @description When a project is created with [make()]
#' or [drake_config()], the project's pseudo-random number generator
#' seed is cached. Then, unless the cache is destroyed,
#' the seeds of all the targets will deterministically depend on
#' this one central seed. That way, reproducibility is protected,
#' even under randomness.
#' @export
#' @return An integer vector.
#'
#' @inheritParams cached
#'
#' @examples
#' cache <- storr::storr_environment() # Just for the examples.
#' my_plan <- drake_plan(
#'   target1 = sqrt(1234),
#'   target2 = sample.int(n = 12, size = 1) + target1
#' )
#' tmp <- sample.int(1) # Needed to get a .Random.seed, but not for drake.
#' digest::digest(.Random.seed) # Fingerprint of the current R session's seed.
#' make(my_plan, cache = cache) # Run the project, build the targets.
#' digest::digest(.Random.seed) # Your session's seed did not change.
#' # drake uses a hard-coded seed if you do not supply one.
#' read_drake_seed(cache = cache)
#' readd(target2, cache = cache) # Randomly-generated target data.
#' clean(target2, cache = cache) # Oops, I removed the data!
#' tmp <- sample.int(1) # Maybe the R session's seed also changed.
#' make(my_plan, cache = cache) # Rebuild target2.
#' # Same as before:
#' read_drake_seed(cache = cache)
#' readd(target2, cache = cache)
#' # You can also supply a seed.
#' # If your project already exists, it must agree with the project's
#' # preexisting seed (default: 0)
#' clean(target2, cache = cache)
#' make(my_plan, cache = cache, seed = 0)
#' read_drake_seed(cache = cache)
#' readd(target2, cache = cache)
#' # If you want to supply a different seed than 0,
#' # you need to destroy the cache and start over first.
#' clean(destroy = TRUE, cache = cache)
#' cache <- storr::storr_environment() # Just for the examples.
#' make(my_plan, cache = cache, seed = 1234)
#' read_drake_seed(cache = cache)
#' readd(target2, cache = cache)
read_drake_seed <- function(
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = 1L
) {
  if (is.null(cache)) {
    cache <- get_cache(path = path, search = search, verbose = verbose)
  }
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  if (cache$exists(key = "seed", namespace = "session")) {
    cache$get(key = "seed", namespace = "session")
  } else {
    stop("Pseudo-random seed not found in the cache.")
  }
}

#' @title Read and return a drake target/import from the cache.
#' @description [readd()] returns an object from the cache,
#' and [loadd()] loads one or more objects from the cache
#' into your environment or session. These objects are usually
#' targets built by [make()].
#' @details There are two uses for the
#' [loadd()] and [readd()] functions:
#' 1. Exploring the results outside the `drake`/`make()` pipeline.
#'   When you call [make()] to run your project,
#'   `drake` puts the targets in a cache, usually a folder called `.drake`.
#'   You may want to inspect the targets afterwards, possibly in an
#'   interactive R session. However, the files in the `.drake` folder
#'   are organized in a special format created by the
#'   [`storr`](https://github.com/richfitz/storr) package,
#'   which is not exactly human-readable.
#'   To retrieve a target for manual viewing, use [readd()].
#'   To load one or more targets into your session, use [loadd()].
#' 2. In `knitr` / R Markdown reports.
#'   You can borrow `drake` targets in your active code chunks
#'   if you have the right calls to [loadd()] and [readd()].
#'   These reports can either run outside the `drake` pipeline,
#'   or better yet, as part of the pipeline itself.
#'   If you call `knitr_in("your_report.Rmd")` inside a [drake_plan()]
#'   command, then [make()] will scan `"your_report.Rmd"` for
#'   calls to `loadd()` and `readd()` in active code chunks,
#'   and then treat those loaded targets as dependencies.
#'   That way, [make()] will automatically (re)run the report if those
#'   dependencies change.
#' @note Please do not put calls to [loadd()] or [readd()] inside
#' your custom (imported) functions or the commands in your [drake_plan()].
#' This creates confusion inside [make()], which has its own ways of
#' interacting with the cache.
#' @seealso [cached()], [drake_plan()], [make()]
#' @export
#' @return The cached value of the `target`.
#' @inheritParams cached
#' @param target If `character_only` is `TRUE`, then
#'   `target` is a character string naming the object to read.
#'   Otherwise, `target` is an unquoted symbol with the name of the
#'   object.
#' @param character_only Logical, whether `name` should be treated
#'   as a character or a symbol
#'   (just like `character.only` in [library()]).
#' @param namespace Optional character string,
#'   name of the `storr` namespace to read from.
#' @param show_source Logical, option to show the command
#'   that produced the target or indicate that the object
#'   was imported (using [show_source()]).
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' readd(reg1) # Return imported object 'reg1' from the cache.
#' readd(small) # Return targets 'small' from the cache.
#' readd("large", character_only = TRUE) # Return 'large' from the cache.
#' # For external files, only the fingerprint/hash is stored.
#' readd(file_store("report.md"), character_only = TRUE)
#' }
#' })
#' }
readd <- function(
  target,
  character_only = FALSE,
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  namespace = NULL,
  verbose = 1L,
  show_source = FALSE
) {
  # if the cache is null after trying get_cache:
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  if (is.null(namespace)) {
    namespace <- cache$default_namespace
  }
  if (show_source) {
    show_source(
      target = target,
      config = list(cache = cache),
      character_only = TRUE
    )
  }
  cache$get(
    standardize_key(target),
    namespace = namespace,
    use_cache = TRUE
  )
}

#' @title Show how a target/import was produced.
#' @description Show the command that produced a target
#'   or indicate that the object or file was imported.
#' @export
#' @param target Symbol denoting the target or import
#'   or a character vector if character_only is `TRUE`.
#' @param config A [drake_config()] list.
#' @param character_only Logical, whether to interpret
#'   `target` as a symbol (`FALSE`) or character vector
#'   (`TRUE`).
#' @examples
#' plan <- drake_plan(x = sample.int(15))
#' cache <- storr::storr_environment() # custom in-memory cache
#' make(plan, cache = cache)
#' config <- drake_config(plan, cache = cache)
#' show_source(x, config)
show_source <- function(target, config, character_only = FALSE) {
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  cache <- config$cache
  meta <- diagnose(target = target, cache = cache, character_only = TRUE)
  prefix <- ifelse(is_encoded_path(target), "File ", "Target ")
  if (meta$imported) {
    message(prefix, target, " was imported.")
  } else {
    command <- gsub("^\\{\n ", "", meta$command)
    command <- gsub(" \n\\}$", "", command)
    message(
      prefix, target, " was built from command:\n  ", target, " = ", command)
  }
}
