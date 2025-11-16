#' get_applicant_countries
#'
#' @keywords internal
#' @noRd
get_applicant_countries <- function(patent_json) {
  applicants <- patent_json$biblio$parties$applicants
  if (!is.null(applicants) && length(applicants) > 0) {
    countries <- sapply(applicants, function(x) {
      country <- if(!is.null(x$residence)) x$residence else NA
      if (is.na(country)) return(NA)
      return(country)
    })
    countries <- countries[!is.na(countries)]
    if (length(countries) > 0) return(paste(countries, collapse = "; "))
  }
  return(NA)
}
