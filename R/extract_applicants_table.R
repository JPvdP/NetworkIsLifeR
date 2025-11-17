#' extract_applicants_table
#'
#' This extracts information about the applicants. It will export one line per applicant
#'
#' @param file_path A path to a jsonl file exported from lens.org
#' @param max_records The maximum number of records to be extracted
#'
#' @return Returns a dataframe with: "lens_id" ,  "sequence" , "name" ,     "residence", "address"
#' @examples
#' example_file <- system.file("extdata", "lens-export-3.jsonl", package = "NetworkIsLifeR")
#' extract_applicants_table(example_file)
#' @export
extract_applicants_table <- function(file_path, max_records = NULL) {
  lines <- base::readLines(file_path, warn = FALSE)
  if (!is.null(max_records)) lines <- head(lines, max_records)

  purrr::map_dfr(lines, function(line) {
    tryCatch({
      patent_json <- jsonlite::fromJSON(line, simplifyVector = FALSE)
      lens_id <- patent_json$lens_id

      applicants <- patent_json$biblio$parties$applicants
      if (!is.null(applicants) && length(applicants) > 0) {
        purrr::map_dfr(seq_along(applicants), function(i) {
          tibble::tibble(
            lens_id = lens_id,
            sequence = i,
            name = safe_extract(applicants[[i]]),
            residence = if(!is.null(applicants[[i]]$residence)) applicants[[i]]$residence else NA,
            address = if(!is.null(applicants[[i]]$extracted_address)) applicants[[i]]$extracted_address else NA
          )
        })
      } else NULL
    }, error = function(e) NULL)
  })
}
