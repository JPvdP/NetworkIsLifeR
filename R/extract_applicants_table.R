#' Extract applicants into a long (one-row-per-applicant) table from patent JSONL
#'
#' @description
#' Reads a JSON Lines (JSONL) file—or, optionally, all JSONL files in a directory—parses each
#' record (one JSON object per line), and extracts applicant-level information from
#' \code{biblio$parties$applicants}. The result is returned as a single aggregated
#' tibble/data.frame with one row per applicant per patent.
#'
#' @details
#' When \code{file_path} is a directory, input files are discovered with \code{list.files()}
#' using \code{pattern} (and optionally \code{recursive}). Files ending in \code{.gz} are read
#' via \code{gzfile()}. Parsing failures are handled per line and (optionally) reported via
#' \code{verbose} without stopping the full run.
#'
#' The argument \code{max_records} limits the number of JSONL lines processed \emph{per file}.
#' The argument \code{max_records_total} caps the \emph{total number of rows returned} across
#' all processed files (useful for sampling large corpora).
#'
#' @param file_path Character scalar. Path to a JSONL/JSONL.GZ file or to a directory containing
#'   multiple JSONL files.
#' @param max_records Integer or \code{NULL}. Maximum number of JSONL records (lines) to process
#'   per file. Default \code{NULL} processes all lines in each file.
#' @param max_records_total Integer or \code{NULL}. Optional global cap on the total number of
#'   rows returned across all files. Default \code{NULL}.
#' @param pattern Character. Regular expression used to select files when \code{file_path} is a
#'   directory. Default matches \code{.jsonl} and \code{.jsonl.gz}.
#' @param recursive Logical. If \code{TRUE}, searches subdirectories when \code{file_path} is a
#'   directory.
#' @param verbose Logical. If \code{TRUE}, prints progress messages and per-line error messages.
#' @param add_source_file Logical. If \code{TRUE}, adds a \code{source_file} column (the basename
#'   of the file that produced each row).
#'
#' @return A tibble/data.frame with one row per applicant per patent. Columns include:
#' \itemize{
#'   \item \code{lens_id}: Lens patent identifier for the record.
#'   \item \code{sequence}: Applicant order within the patent record.
#'   \item \code{name}: Extracted applicant name (via \code{safe_extract()}).
#'   \item \code{residence}: Applicant residence, when available.
#'   \item \code{address}: Extracted address, when available.
#'   \item \code{source_file}: Present only when \code{add_source_file = TRUE}.
#' }
#'
#' @section Dependencies:
#' Requires \pkg{jsonlite}, \pkg{purrr}, \pkg{tibble}, and \pkg{dplyr}. This function also expects
#' \code{safe_extract()} to be available in scope (typically exported or internal to the package).
#'
#' @seealso \code{\link[jsonlite:fromJSON]{jsonlite::fromJSON}},
#'   \code{\link[purrr:map_dfr]{purrr::map_dfr}},
#'   \code{\link[dplyr:bind_rows]{dplyr::bind_rows}}
#'
#' @examples
#' \dontrun{
#' # Single file
#' apps1 <- extract_applicants_table("data/patents.jsonl", max_records = 1000)
#'
#' # Directory (aggregate across many files)
#' apps_all <- extract_applicants_table("data/jsonl_batches")
#'
#' # Directory with provenance + global cap
#' apps_cap <- extract_applicants_table("data/jsonl_batches",
#'                                      max_records_total = 5000,
#'                                      add_source_file = TRUE)
#' }
#'
#' @export
extract_applicants_table <- function(file_path,
                                     max_records = NULL,        # per file
                                     max_records_total = NULL,  # optional global cap on rows returned
                                     pattern = "\\.jsonl(\\.gz)?$",
                                     recursive = FALSE,
                                     verbose = TRUE,
                                     add_source_file = FALSE) {

  # Dependencies (fail fast)
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package 'purrr' is required.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.", call. = FALSE)
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required.", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.", call. = FALSE)
  }

  if (missing(file_path) || is.null(file_path) || !nzchar(file_path)) {
    stop("`file_path` must be a non-empty file or directory path.", call. = FALSE)
  }

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

  # Process a single file
  process_one_file <- function(fp, n_limit = NULL) {
    if (verbose) message("Reading file: ", fp)
    lines <- read_lines_any(fp)
    if (!is.null(n_limit)) lines <- head(lines, n_limit)

    if (verbose) message(sprintf("Processing %d records from %s ...", length(lines), basename(fp)))

    out <- purrr::map_dfr(seq_along(lines), function(li) {
      if (verbose && li %% 100 == 0) {
        message(sprintf("  %s: processed %d/%d", basename(fp), li, length(lines)))
      }

      tryCatch({
        patent_json <- jsonlite::fromJSON(lines[[li]], simplifyVector = FALSE)
        lens_id <- patent_json$lens_id

        applicants <- patent_json$biblio$parties$applicants

        if (!is.null(applicants) && length(applicants) > 0) {
          purrr::map_dfr(seq_along(applicants), function(i) {
            tibble::tibble(
              lens_id  = lens_id,
              sequence = i,
              name     = safe_extract(applicants[[i]], "extracted_name", "value"),
              residence = if (!is.null(applicants[[i]]$residence)) applicants[[i]]$residence else NA,
              address   = if (!is.null(applicants[[i]]$extracted_address)) applicants[[i]]$extracted_address else NA
            )
          })
        } else {
          NULL
        }
      }, error = function(e) {
        if (verbose) message(sprintf("Error processing %s line %d: %s", basename(fp), li, e$message))
        NULL
      })
    })

    if (add_source_file && nrow(out) > 0) {
      out$source_file <- basename(fp)
    }

    out
  }

  # Iterate files with optional global cap
  total_remaining <- max_records_total
  results <- vector("list", length(files))

  for (idx in seq_along(files)) {
    fp <- files[[idx]]

    if (!is.null(total_remaining) && total_remaining <= 0) break

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

    res <- process_one_file(fp, n_limit = per_file_limit)
    results[[idx]] <- res

    if (!is.null(total_remaining)) {
      total_remaining <- total_remaining - nrow(res)
    }
  }

  dplyr::bind_rows(results)
}
