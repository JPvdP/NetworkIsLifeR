#' extract_classifications_table_CPC
#'
#' This extracts information from a lens.org patent export
#'
#' @param file_path A path to a jsonl file exported from lens.org
#' @param max_records The maximum number of records to be extracted
#'
#' @return Returns a dataframe with: "lens_id"  , "Classification_system", "Symbol", "Classification_value", "Sequence"
#' classification_system: CPCR, for the cooperative patent classification system.
#' The field 'Symbol' gives the class.
#' The field 'classification_value' can take values like I or L. From the Lens docs:
#' I stands for “Invention”.
#' L stands for “Later”.
#' So if you see a classification entry with "classification_value": "I", it means that that classification symbol is flagged as relating to the invention as determined by the patent office/ Lens — i.e., the classification is considered part of the core inventive subject matter.
#' If it were "L", it would be a classification assigned later (for example: a supplementary, non-primary classification added after initial assignment) rather than necessarily part of the primary inventive content.
#' The field 'Sequence' gives the position of the symbol in the chain of classifications
#' @examples
#' example_file <- system.file("extdata", "lens-export-3.jsonl", package = "NetworkIsLifeR")
#' plop <- extract_classifications_table(example_file)
#' @export
extract_classifications_table_CPC <- function(file_path, max_records = NULL) {
  lines <- base::readLines(file_path, warn = FALSE)
  if (!is.null(max_records)) lines <- head(lines, max_records)

  purrr::map_dfr(lines, function(line) {
    tryCatch({
      patent_json <- jsonlite::fromJSON(line, simplifyVector = FALSE)
      lens_id <- patent_json$lens_id

      # ---- this is the correct path for CPC in your sample ----
      cpc <- patent_json$biblio$classifications_cpc$classifications

      if (!is.null(cpc) && length(cpc) > 0) {
        purrr::map_dfr(seq_along(cpc), function(i) {
          tibble::tibble(
            lens_id = lens_id,
            classification_system = "CPC",
            symbol = if (!is.null(cpc[[i]]$symbol)) cpc[[i]]$symbol else NA,
            classification_value = if (!is.null(cpc[[i]]$classification_value)) cpc[[i]]$classification_value else NA,
            classification_symbol_position = if (!is.null(cpc[[i]]$classification_symbol_position)) cpc[[i]]$classification_symbol_position else NA,
            sequence = i
          )
        })
      } else NULL
    }, error = function(e) NULL)
  })
}
