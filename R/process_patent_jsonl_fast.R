#' process_patent_jsonl_fast
#'
#' This extracts information from a lens.org patent export
#'
#' @param file_path A path to a jsonl file exported from lens.org.
#' @param max_records The maximum number of records to be extracted.
#' @param verbose Extra information return from the function.
#' @importFrom utils head
#' @importFrom jsonlite fromJSON
#' @return Returns a dataframe with: "lens_id"  ,            "jurisdiction"     ,    "doc_number" ,          "kind"
#' "date_published"  ,     "lang"  ,               "title"      ,          "abstract",
#' "app_doc_number"  ,     "app_date"   ,          "priority_date" ,        "applicants" ,
#' "inventors"    ,        "ipc_classifications",   "patent_status"    ,    "granted" ,
#' "grant_date"    ,       "cited_by_count" ,      "simple_family_size" ,  "extended_family_size"
#' @examples
#' library(purrr)
#' library(dplyr)
#' example_file <- system.file("extdata", "lens-export-3.jsonl", package = "NetworkIsLifeR")
#' process_patent_jsonl_fast(example_file)
#' @export
process_patent_jsonl_fast <- function(file_path, max_records = NULL, verbose = TRUE) {

  # ---- Open connection (handles .gz or .jsonl automatically) ----
  if (verbose) message("Opening file connection...")

  con <- if (grepl("\\.gz$", file_path)) {
    gzfile(file_path, open = "r")
  } else {
    file(file_path, open = "r")
  }

  on.exit(close(con), add = TRUE)

  # ---- Initialize list to collect processed results ----
  results <- list()
  counter <- 0

  if (verbose) message("Streaming and processing records...")

  # ---- Read file line by line ----
  repeat {
    line <- readLines(con, n = 1, warn = FALSE)

    if (length(line) == 0) break  # End of file

    counter <- counter + 1

    # Stop if max_records reached
    if (!is.null(max_records) && counter > max_records) break

    # Progress indicator
    if (verbose && counter %% 1000 == 0) {
      message(sprintf("Processed %d records...", counter))
    }

    # Parse and extract
    result <- tryCatch({
      json_obj <- jsonlite::fromJSON(line, simplifyVector = FALSE)
      extract_patent_info(json_obj)

    }, error = function(e) {
      if (verbose) message(sprintf("Error at record %d: %s", counter, e$message))
      NULL
    })

    results[[counter]] <- result
  }

  if (verbose) message(sprintf("Finished. Total processed: %d", counter))

  # ---- Bind rows safely ----
  dplyr::bind_rows(results)
}
