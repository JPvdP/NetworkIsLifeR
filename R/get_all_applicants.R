#' get_all_applicants
#'
#' @keywords internal
#' @noRd
get_all_applicants <- function(patent_json) {
  applicants <- patent_json$biblio$parties$applicants
  if (!is.null(applicants) && length(applicants) > 0) {
    names <- sapply(applicants, function(x) {
      name <- safe_extract(x, "extracted_name", "value")
      if (is.na(name)) return(NA)
      return(name)
    })
    names <- names[!is.na(names)]
    if (length(names) > 0) return(paste(names, collapse = "; "))
  }
  return(NA)
}
