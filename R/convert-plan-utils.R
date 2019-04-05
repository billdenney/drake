drake_plan_call <- function(plan) {
  target_calls <- drake_pmap(plan, drake_target_call)
  names(target_calls) <- plan$target
  as.call(c(quote(drake_plan), target_calls))
}

drake_target_call <- function(...) {
  args <- select_valid(list(...))
  target <- parse(text = args$target)[[1]]
  args$target <- NULL
  if (is.character(args[["command"]])) {
    args$command <- safe_parse(args[["command"]])
  }
  if ("trigger" %in% names(args)) {
    if (is.character(args[["trigger"]])) {
      args[["trigger"]] <- safe_parse(args[["trigger"]])
    }
  }
  if (!identical(names(args), "command")) {
    args[["command"]] <- as.call(c(quote(target), args))
  }
  args[["command"]]
}

node_plan <- function(node) {
  weak_tibble(
    target = safe_deparse(node@code[[2]]),
    command = safe_deparse(node@code[[3]])
  )
}

plan_to_text <- function(plan) {
  . <- NULL
  graph <- drake_config(
    plan[, c("target", "command")],
    envir = new.env(parent = emptyenv()),
    cache = storr::storr_environment(),
    verbose = FALSE
  )$graph
  order <- igraph::topo_sort(graph)$name
  order <- intersect(order, plan$target)
  order <- match(order, table = plan$target)
  plan <- plan[order, ]
  if (!is.character(plan$command)) {
    plan$command <- vapply(plan$command,
                           safe_deparse,
                           FUN.VALUE = character(1))
  }
  text <- paste(plan$target, "<-", plan$command)
  if (requireNamespace("styler")) {
    text <- styler::style_text(text)
  }
  text
}

style_leaf <- function(name, expr, append_comma) {
  text <- styler::style_text(safe_deparse(expr))
  text[1] <- paste(name, "=", text[1])
  if (append_comma) {
    text[length(text)] <- paste0(text[length(text)], ",")
  }
  text
}

style_recursive <- function(expr, name, append_comma) {
  text <- style_recursive_loop(expr)
  head <- character(0)
  if (nzchar(name)) {
    head <- paste(name, "= ")
  }
  head <- paste0(head, safe_deparse(expr[[1]]), "(")
  out <- c(head, paste0("  ", text), ")")
  if (append_comma) {
    out[length(out)] <- paste0(out[length(out)], ",")
  }
  out
}

style_recursive_loop <- function(expr) {
  args <- expr[-1]
  text <- character(0)
  for (i in seq_along(args)) {
    recurse <- is_target_call(args[[i]]) || is_trigger_call(args[[i]])
    if (recurse) {
      text <- c(
        text,
        style_recursive(
          expr = args[[i]],
          name = names(args)[i],
          append_comma = i < length(args)
        )
      )
    } else {
      text <- c(
        text,
        style_leaf(
          expr = args[[i]],
          name = names(args)[i],
          append_comma = i < length(args)
        )
      )
    }
  }
  text
}
