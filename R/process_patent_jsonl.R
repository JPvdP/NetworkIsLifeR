#' Process patent JSONL files into a tidy data frame
#'
#' @description
#' Reads one JSON Lines (JSONL) file—or, optionally, all JSONL files in a directory—parses each
#' record (one JSON object per line), applies \code{extract_patent_info()} to extract a standard
#' set of fields, and returns a single aggregated tibble/data.frame. Parsing errors per line are
#' caught and reported (when \code{verbose = TRUE}) without aborting the full run.
#'
#' @details
#' When \code{file_path} is a directory, files are discovered via \code{list.files()} using
#' \code{pattern} (and optionally \code{recursive}). Files ending in \code{.gz} are read via
#' \code{gzfile()}; other files are read with \code{readLines()}.
#'
#' The argument \code{max_records} limits the number of JSONL lines processed \emph{per file}.
#' The argument \code{max_records_total} caps the \emph{total number of rows returned} across
#' all processed files (useful for sampling large corpora).
#'
#' @param file_path Character scalar. Path to a JSONL/JSONL.GZ file or to a directory containing
#'   multiple JSONL files.
#' @param max_records Integer or \code{NULL}. Maximum number of records (lines) to process
#'   per file. Default \code{NULL} processes all lines.
#' @param max_records_total Integer or \code{NULL}. Optional global cap on the total number of
#'   rows returned across all files. Default \code{NULL}.
#' @param verbose Logical. If \code{TRUE}, prints progress messages and per-line errors.
#' @param pattern Character. Regular expression used to match JSONL files when \code{file_path}
#'   is a directory. Default matches \code{.jsonl} and \code{.jsonl.gz}.
#' @param recursive Logical. If \code{TRUE}, searches subdirectories when \code{file_path} is a
#'   directory.
#' @param add_source_file Logical. If \code{TRUE}, adds a \code{source_file} column (the basename
#'   of the file that produced each row).
#'
#' @return A tibble/data.frame with one row per successfully parsed JSONL record, as returned by
#'   \code{extract_patent_info()}. If \code{add_source_file = TRUE}, includes a \code{source_file}
#'   column.
#'
#' @section Dependencies:
#' Requires \pkg{jsonlite} for JSON parsing and \pkg{purrr} for mapping. Uses \code{dplyr::bind_rows()}
#' to aggregate outputs.
#'
#' @seealso \code{\link[jsonlite:fromJSON]{jsonlite::fromJSON}},
#'   \code{\link[purrr:map_dfr]{purrr::map_dfr}},
#'   \code{\link[dplyr:bind_rows]{dplyr::bind_rows}}
#'
#' @examples
#' \dontrun{
#' # Single file
#' df1 <- process_patent_jsonl("data/patents.jsonl", max_records = 500)
#'
#' # Directory (aggregate all files)
#' df_all <- process_patent_jsonl("data/jsonl_batches")
#'
#' # Directory with global cap + provenance
#' df_cap <- process_patent_jsonl("data/jsonl_batches",
#'                                max_records_total = 10000,
#'                                add_source_file = TRUE)
#' }
#'
#' @export
process_patent_jsonl <- function(file_path,
                                 max_records = NULL,        # per file limit (keeps backward compatibility)
                                 max_records_total = NULL,  # optional global limit across all files
                                 verbose = TRUE,
                                 pattern = "\\.jsonl(\\.gz)?$",  # which files to read when file_path is a folder
                                 recursive = FALSE,
                                 add_source_file = FALSE) {

  # Dependencies (fail fast with clear message)
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package 'purrr' is required.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.", call. = FALSE)
  }

  if (missing(file_path) || is.null(file_path) || !nzchar(file_path)) {
    stop("`file_path` must be a non-empty file or directory path.", call. = FALSE)
  }

  # Resolve whether we got a file or a directory
  is_dir  <- dir.exists(file_path)
  is_file <- file.exists(file_path)

  if (!is_dir && !is_file) {
    stop("`file_path` does not exist as a file or directory: ", file_path, call. = FALSE)
  }

  files <- if (is_dir) {
    list.files(file_path, pattern = pattern, full.names = TRUE, recursive = recursive)
  } else {
    file_path
  }

  if (length(files) == 0) {
    stop("No files matched `pattern` in directory: ", file_path, call. = FALSE)
  }

  # Helper to read lines, including .gz
  read_lines_any <- function(fp) {
    if (grepl("\\.gz$", fp, ignore.case = TRUE)) {
      base::readLines(gzfile(fp), warn = FALSE)
    } else {
      base::readLines(fp, warn = FALSE)
    }
  }

  # Helper to process one file
  process_one_file <- function(fp, n_limit = NULL, show_progress = TRUE) {
    if (show_progress) message("Reading file: ", fp)
    lines <- read_lines_any(fp)

    if (!is.null(n_limit)) {
      lines <- head(lines, n_limit)
    }

    if (show_progress) message(sprintf("Processing %d records from %s ...", length(lines), basename(fp)))

    out <- purrr::map_dfr(seq_along(lines), function(i) {
      if (show_progress && i %% 100 == 0) {
        message(sprintf("  %s: processed %d/%d", basename(fp), i, length(lines)))
      }

      tryCatch({
        patent_json <- jsonlite::fromJSON(lines[i], simplifyVector = FALSE)
        extract_patent_info(patent_json)
      }, error = function(e) {
        message(sprintf("Error processing %s line %d: %s", basename(fp), i, e$message))
        NULL
      })
    })

    if (add_source_file && nrow(out) > 0) {
      out$source_file <- basename(fp)
    }

    out
  }

  # Main loop (needed to support a global cap cleanly)
  total_remaining <- max_records_total
  results <- vector("list", length(files))
  names(results) <- files

  for (idx in seq_along(files)) {
    fp <- files[[idx]]

    if (!is.null(total_remaining) && total_remaining <= 0) {
      break
    }

    # Determine per-file limit for this file
    per_file_limit <- max_records
    if (!is.null(total_remaining)) {
      if (is.null(per_file_limit)) {
        per_file_limit <- total_remaining
      } else {
        per_file_limit <- min(per_file_limit, total_remaining)
      }
    }

    if (verbose && length(files) > 1) {
      message(sprintf("File %d/%d", idx, length(files)))
    }

    res <- process_one_file(fp, n_limit = per_file_limit, show_progress = verbose)
    results[[idx]] <- res

    if (!is.null(total_remaining)) {
      total_remaining <- total_remaining - nrow(res)
    }
  }

  patent_data <- dplyr::bind_rows(results)

  if (verbose) message("Done! Rows returned: ", nrow(patent_data))
  patent_data
}
