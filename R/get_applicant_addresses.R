#' get_applicant_addresses
#'
#' @keywords internal
#' @noRd
get_applicant_addresses <- function(patent_json) {
  applicants <- patent_json$biblio$parties$applicants
  if (!is.null(applicants) && length(applicants) > 0) {
    addresses <- sapply(applicants, function(x) {
      addr <- if(!is.null(x$extracted_address)) x$extracted_address else NA
      if (is.na(addr)) return(NA)
      return(addr)
    })
    addresses <- addresses[!is.na(addresses)]
    if (length(addresses) > 0) return(paste(addresses, collapse = "; "))
  }
  return(NA)
}
