#' extract_classifications_table
#'
#' This extracts information from a lens.org patent export
#'
#' @param file_path A path to a jsonl file exported from lens.org
#' @param max_records The maximum number of records to be extracted
#'
#' @return Returns a dataframe with: "lens_id"  , "Classification_system", "Symbol", "Classification_value", "Sequence"
#' classification_system: IPCR for the international patent classification system, or CPCR, for the cooperative patent classification system.
#' The field 'Symbol' gives the class.
#' The field 'classification_value' (for CPC or IPRC classifications) can take values like I or L. From the Lens docs:
#' I stands for “Invention”.
#' L stands for “Later”.
#' So if you see a classification entry with "classification_value": "I", it means that that classification symbol is flagged as relating to the invention as determined by the patent office/ Lens — i.e., the classification is considered part of the core inventive subject matter.
#' If it were "L", it would be a classification assigned later (for example: a supplementary, non-primary classification added after initial assignment) rather than necessarily part of the primary inventive content.
#' The field 'Sequence' gives the position of the symbol in the chain of classifications
#' @examples
#' example_file <- system.file("extdata", "lens-export-3.jsonl", package = "NetworkIsLifeR")
#' plop <- extract_classifications_table(example_file)
#' @export
extract_classifications_table <- function(file_path, max_records = NULL) {
  lines <- base::readLines(file_path, warn = FALSE)
  if (!is.null(max_records)) lines <- head(lines, max_records)

  purrr::map_dfr(lines, function(line) {
    tryCatch({
      patent_json <- fromJSON(line, simplifyVector = FALSE)
      lens_id <- patent_json$lens_id

      ipcr <- patent_json$biblio$classifications_ipcr$classifications
      if (!is.null(ipcr) && length(ipcr) > 0) {
       purrr::map_dfr(seq_along(ipcr), function(i) {
          tibble::tibble(
            lens_id = lens_id,
            classification_system = "IPCR",
            symbol = if(!is.null(ipcr[[i]]$symbol)) ipcr[[i]]$symbol else NA,
            classification_value = if(!is.null(ipcr[[i]]$classification_value)) ipcr[[i]]$classification_value else NA,
            sequence = i
          )
        })
      } else NULL
    }, error = function(e) NULL)
  })
}
