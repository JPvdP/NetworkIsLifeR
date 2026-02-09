#' List Lens.org JSONL/JSONL.GZ export files in a directory
#'
#' Convenience helper to list Lens.org export files in a folder. Files are
#' matched by extension (`.jsonl` or `.jsonl.gz`) and returned as full paths.
#'
#' The function prints a short summary of the files found to the console.
#'
#' @param directory A path to a directory to scan for Lens.org export files.
#'
#' @return A character vector of full file paths. If no matching files are found,
#'   returns `character(0)`.
#'
#' @details
#' Files are detected using [base::list.files()] with pattern
#' `\\.(jsonl|jsonl\\.gz)$`.
#'
#' @examples
#' \dontrun{
#' files <- list_lens_files("data/lens_exports/")
#' files
#' }
#'
#' @export
list_lens_files <- function(directory) {
  if (!dir.exists(directory)) {
    stop("Directory not found: ", directory)
  }

  files <- list.files(directory,
                      pattern = "\\.(jsonl|jsonl\\.gz)$",
                      full.names = TRUE)

  if (length(files) == 0) {
    cat("No .jsonl or .jsonl.gz files found in:", directory, "\n")
    return(character(0))
  }

  cat("Found", length(files), "JSONL file(s):\n")
  for (i in seq_along(files)) {
    cat(sprintf("[%d] %s\n", i, basename(files[i])))
  }

  return(files)
}
