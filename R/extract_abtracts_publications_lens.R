#' Extract patent abstracts from Lens.org JSONL/JSONL.GZ exports
#'
#' Read a single Lens.org patent export file (`.jsonl` or `.jsonl.gz`) **or**
#' a directory containing multiple such files, and return a combined data frame
#' of abstracts keyed by Lens identifier.
#'
#' When `file_path` is a directory, all files matching `\\.(jsonl|jsonl\\.gz)$`
#' are processed and row-bound in the order returned by [base::list.files()].
#'
#' @param file_path A path to a `.jsonl` / `.jsonl.gz` file, or a directory
#'   containing one or more such files.
#'
#' @return A data frame (typically a tibble) with columns:
#'   \describe{
#'     \item{lens_id}{Character. Lens patent identifier.}
#'     \item{abstract}{Character. Abstract text (may be `NA` if missing).}
#'     \item{year}{Integer. Year extracted from the record (may be `NA` if missing).}
#'   }
#'
#' @details
#' This function delegates the actual parsing of an individual file to
#' `extract_abstracts_single()`, which must be available in the package namespace
#' (commonly as an internal helper).
#'
#' For directory inputs, the function prints basic progress information to the
#' console (via [base::cat()]).
#'
#' @examples
#' \dontrun{
#' # Single file
#' df <- extract_abstract_publications_lens("data/lens_patents.jsonl.gz")
#'
#' # Directory with many exports
#' df_all <- extract_abstract_publications_lens("data/lens_exports/")
#'
#' # Inspect
#' head(df_all)
#' }
#'
#' @seealso `extract_abstracts_single()` (internal helper used per file).
#'
#' @importFrom dplyr bind_rows
#' @export
extract_abstract_publications_lens <- function(file_path) {

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
    all_abstracts <- lapply(files, function(f) {
      cat("Processing:", basename(f), "\n")
      extract_abstracts_single(f)
    })

    return(bind_rows(all_abstracts))
  }

  # Single file processing
  return(extract_abstracts_single(file_path))
}
