#' scale_x_reordered
#'
#' Cleans up labels from reorder_within().
#'
#' @keywords internal
scale_x_reordered <- function(sep = "___") {
  ggplot2::scale_x_discrete(labels = function(x) gsub(paste0(sep, ".*$"), "", x))
}
