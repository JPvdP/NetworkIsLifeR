#' extract_inventors_table
#'
#' This extracts information about the inventors. It will export one line per inventors.
#'
#' @param file_path A path to a jsonl file exported from lens.org
#' @param max_records The maximum number of records to be extracted
#'
#' @return Returns a dataframe with: "lens_id" ,  "sequence" , "name" ,     "residence", "address"
#' @examples
#' example_file <- system.file("extdata", "lens-export-3.jsonl", package = "NetworkIsLifeR")
#' extract_inventors_table(example_file)
#' @export
extract_inventors_table <- function(file_path, max_records = NULL) {
lines <- base::readLines(file_path, warn = FALSE)
if (!is.null(max_records)) lines <- head(lines, max_records)

purrr::map_dfr(lines, function(line) {
  tryCatch({
    patent_json <- jsonlite::fromJSON(line, simplifyVector = FALSE)
    lens_id <- patent_json$lens_id

    inventors <- patent_json$biblio$parties$inventors
    if (!is.null(inventors) && length(inventors) > 0) {
      purrr::map_dfr(seq_along(inventors), function(i) {
        tibble::tibble(
          lens_id = lens_id,
          sequence = if(!is.null(inventors[[i]]$sequence)) inventors[[i]]$sequence else i,
          name = safe_extract(inventors[[i]], "extracted_name", "value"),
          residence = if(!is.null(inventors[[i]]$residence)) inventors[[i]]$residence else NA
        )
      })
    } else NULL
  }, error = function(e) NULL)
})
}
