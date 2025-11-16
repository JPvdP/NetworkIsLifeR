#' get_all_classifications
#'
#' @keywords internal
#' @noRd
get_all_classifications <- function(patent_json) {
  ipcr <- patent_json$biblio$classifications_ipcr$classifications
  if (!is.null(ipcr) && length(ipcr) > 0) {
    symbols <- sapply(ipcr, function(x) if(!is.null(x$symbol)) x$symbol else NA)
    paste(symbols[!is.na(symbols)], collapse = "; ")
  } else NA
}
