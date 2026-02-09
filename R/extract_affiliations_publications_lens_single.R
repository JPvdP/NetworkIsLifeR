#' Extract publication affiliations in a simplified table
#'
#' Read a single Lens.org publications export file (`.jsonl` or `.jsonl.gz`) **or**
#' a directory containing multiple such files, and return a combined data frame
#' with affiliations in a reduced set of fields.
#'
#' When `file_path` is a directory, all files matching `\\.(jsonl|jsonl\\.gz)$`
#' are processed and row-bound in the order returned by [base::list.files()].
#'
#' @param file_path A path to a `.jsonl` / `.jsonl.gz` file, or a directory
#'   containing one or more such files.
#'
#' @return A data frame (typically a tibble) with columns:
#' \describe{
#'   \item{lens_id}{Character. Lens publication identifier.}
#'   \item{year}{Integer. Publication year if available, otherwise `NA`.}
#'   \item{author_name}{Character. Author display name (constructed when missing).}
#'   \item{affiliation_name}{Character. Affiliation name if present.}
#'   \item{city}{Character. Affiliation city if present.}
#'   \item{country}{Character. Affiliation country if present.}
#' }
#'
#' @details
#' This function delegates parsing of each individual file to
#' `extract_affiliations_simple_single()`, which must be available in the package
#' namespace (commonly as an internal helper).
#'
#' For directory inputs, the function prints basic progress information to the
#' console (via [base::cat()]).
#'
#' @examples
#' \dontrun{
#' # Single file
#' aff <- extract_affiliations_publications_lens_single("data/lens_publications.jsonl.gz")
#'
#' # Directory with many exports
#' aff_all <- extract_affiliations_publications_lens_single("data/lens_publications_exports/")
#' }
#'
#' @seealso `extract_affiliations_simple_single()` (internal helper used per file).
#'
#' @importFrom dplyr bind_rows
#' @export
extract_affiliations_publications_lens_single <- function(file_path) {

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
      extract_affiliations_simple_single(f)
    })

    return(bind_rows(all_affiliations))
  }

  # Single file processing
  return(extract_affiliations_simple_single(file_path))
}
