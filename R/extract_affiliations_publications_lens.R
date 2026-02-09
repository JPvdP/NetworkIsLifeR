#' Extract publication affiliations from Lens.org JSONL/JSONL.GZ exports
#'
#' Read a single Lens.org publications export file (`.jsonl` or `.jsonl.gz`) **or**
#' a directory containing multiple such files, and return a combined data frame
#' with affiliation information extracted from the records.
#'
#' When `file_path` is a directory, all files matching `\\.(jsonl|jsonl\\.gz)$`
#' are processed and row-bound in the order returned by [base::list.files()].
#'
#' @param file_path A path to a `.jsonl` / `.jsonl.gz` file, or a directory
#'   containing one or more such files.
#' @param expand_authors Logical. If `TRUE`, returns one row per
#'   author–affiliation combination (as produced by the underlying single-file
#'   parser). If `FALSE`, returns the less-expanded structure defined by
#'   `extract_affiliations_single()`.
#'
#' @return A data frame (typically a tibble) containing affiliation information.
#'   The exact columns depend on `extract_affiliations_single()` and the Lens
#'   export schema.
#'
#' @details
#' This function delegates parsing of each individual file to
#' `extract_affiliations_single()`, which must be available in the package
#' namespace (commonly as an internal helper).
#'
#' For directory inputs, the function prints basic progress information to the
#' console (via [base::cat()]).
#'
#' @examples
#' \dontrun{
#' # Single file
#' aff <- extract_affiliations_publications_lens(
#'   "data/lens_publications.jsonl.gz",
#'   expand_authors = TRUE
#' )
#'
#' # Directory with many exports
#' aff_all <- extract_affiliations_publications_lens(
#'   "data/lens_publications_exports/",
#'   expand_authors = TRUE
#' )
#' }
#'
#' @seealso `extract_affiliations_single()` (internal helper used per file).
#'
#' @importFrom dplyr bind_rows
#' @export
extract_affiliations_publications_lens <- function(file_path, expand_authors = TRUE) {

  # Check if file/directory exists
  if (!file.exists(file_path)) {
    stop("File or directory not found: ", file_path)
  }

  # If it's a directory, process all JSONL files in it
  if (dir.exists(file_path)) {
    files <- list.files(file_path,
                        pattern = "\\.(jsonl|jsonl\\.gz)$",
                        full.names = TRUE)

    if (length(files) == 0) {
      stop("No .jsonl or .jsonl.gz files found in directory: ", file_path)
    }

    cat("Found", length(files), "file(s) in directory:\n")
    cat(paste("-", basename(files), collapse = "\n"), "\n\n")

    # Process all files and combine
    all_affiliations <- lapply(files, function(f) {
      cat("Processing:", basename(f), "\n")
      extract_affiliations_single(f, expand_authors)
    })

    return(bind_rows(all_affiliations))
  }

  # Single file processing
  return(extract_affiliations_single(file_path, expand_authors))
}
