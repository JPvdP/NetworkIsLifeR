#' Extract abstracts from a single Lens.org JSONL/JSONL.GZ export
#'
#' Internal helper used by [extract_abstract_publications_lens()] to parse one `.jsonl` or
#' `.jsonl.gz` file and return a data frame of `(lens_id, abstract, year)`.
#'
#' The function reads the file line-by-line, attempts to parse each line as JSON,
#' drops malformed records, extracts the abstract and publication year (from
#' `year_published` or the year component of `date_published`), and removes rows
#' without an abstract.
#'
#' @param file_path A path to a single `.jsonl` or `.jsonl.gz` file.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{lens_id}{Character. Lens patent identifier.}
#'   \item{abstract}{Character. Abstract text.}
#'   \item{year}{Integer. Publication year if available, otherwise `NA`.}
#' }
#'
#' @details
#' This is an internal function and is not exported. It is intended to be called
#' by [extract_abstract_publications_lens()] for each file encountered.
#'
#' @keywords internal
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' # Typically not called directly by users:
#' df <- extract_abstracts_single("data/lens_patents.jsonl.gz")
#' }
extract_abstracts_single <- function(file_path) {

  # Read the JSONL file (handles both .jsonl and .jsonl.gz)
  con <- if (grepl("\\.gz$", file_path)) {
    gzfile(file_path, "rt")
  } else {
    file(file_path, "rt")
  }

  # Read lines and parse JSON
  lines <- readLines(con, warn = FALSE)
  close(con)

  # Parse each line as JSON
  records <- lapply(lines, function(x) {
    tryCatch(
      fromJSON(x, flatten = TRUE),
      error = function(e) NULL
    )
  })

  # Remove NULL entries (failed parsing)
  records <- records[!sapply(records, is.null)]

  # Extract relevant fields
  abstracts_df <- data.frame(
    lens_id = sapply(records, function(x) ifelse(is.null(x$lens_id), NA, x$lens_id)),
    abstract = sapply(records, function(x) ifelse(is.null(x$abstract), NA, x$abstract)),
    year = sapply(records, function(x) {
      # Try different year fields that might exist
      if (!is.null(x$year_published)) return(x$year_published)
      if (!is.null(x$date_published)) {
        # Extract year from date if it's a full date
        return(as.integer(substr(x$date_published, 1, 4)))
      }
      return(NA)
    }),
    stringsAsFactors = FALSE
  )

  # Remove rows with no abstract
  abstracts_df <- abstracts_df[!is.na(abstracts_df$abstract), ]

  return(abstracts_df)
}
