#' safe_extract_lens_pats
#'
#' @keywords internal
#' @noRd
safe_extract_lens_pats <- function(obj, ...) {
path <- list(...)
result <- obj
for (element in path) {
  if (is.null(result)) return(NA)
  if (is.list(result) && !is.null(names(result))) {
    result <- result[[element]]
  } else {
    return(NA)
  }
}
if (is.null(result) || length(result) == 0) return(NA)
return(result)
}
