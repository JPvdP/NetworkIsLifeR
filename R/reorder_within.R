#' reorder_within
#'
#' Reorder factor levels within each topic panel.
#' Based on tidytext's reorder_within().
#'
#' @keywords internal
reorder_within <- function(x, by, within, fun = mean, sep = "___") {
  new <- paste(x, within, sep = sep)
  stats::reorder(new, by, FUN = fun)
}
