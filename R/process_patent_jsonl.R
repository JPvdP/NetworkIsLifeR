#' process_patent_jsonl
#'
#' This extracts information from a lens.org patent export
#'
#' @param file_path A path to a jsonl file exported from lens.org
#' @param max_records The maximum number of records to be extracted
#'
#' @return Returns a dataframe with: "lens_id"  ,            "jurisdiction"     ,    "doc_number" ,          "kind"
#' "date_published"  ,     "lang"  ,               "title"      ,          "abstract",
#' "app_doc_number"  ,     "app_date"   ,          "priority_date" ,        "applicants" ,
#' "inventors"    ,        "ipc_classifications",   "patent_status"    ,    "granted" ,
#' "grant_date"    ,       "cited_by_count" ,      "simple_family_size" ,  "extended_family_size"
#' @examples
#' library(purrr)
#' library(tidyverse)
#' example_file <- system.file("extdata", "lens-export-3.jsonl", package = "NetworkIsLifeR")
#' process_patent_jsonl(example_file)
#' @export
process_patent_jsonl <- function(file_path, max_records = NULL, verbose = TRUE) {
# Read JSONL file line by line
if (verbose) message("Reading file...")
lines <- base::readLines(file_path, warn = FALSE)

# Optionally limit number of records
if (!is.null(max_records)) {
  lines <- base::head(lines, max_records)
}

if (verbose) message(base::sprintf("Processing %d records...", length(lines)))

# Parse each line and extract patent info
patent_data <- purrr::map_dfr(seq_along(lines), function(i) {
  if (verbose && i %% 100 == 0) {
    message(base::sprintf("Processed %d/%d records", i, length(lines)))
  }

  tryCatch({
    patent_json <- jsonlite::fromJSON(lines[i], simplifyVector = FALSE)
    extract_patent_info(patent_json)
  }, error = function(e) {
    message(sprintf("Error processing line %d: %s", i, e$message))
    NULL
  })
})

if (verbose) message("Done!")
return(patent_data)
}
