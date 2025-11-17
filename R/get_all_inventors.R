#' get_all_inventors
#'
#' @keywords internal
#' @noRd
get_all_inventors <- function(patent_json) {
  inventors <- patent_json$biblio$parties$inventors
  if (!is.null(inventors) && length(inventors) > 0) {
    names <- sapply(inventors, function(x) {
      name <- safe_extract(x, "extracted_name", "value")
      if (is.na(name)) return(NA)
      return(name)
    })
    names <- names[!is.na(names)]
    if (length(names) > 0) return(paste(names, collapse = "; "))
  }
  return(NA)
}
