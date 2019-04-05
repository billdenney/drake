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
