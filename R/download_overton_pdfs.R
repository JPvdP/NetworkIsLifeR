#' Download PDF files from an Overton export
#'
#' Reads a CSV export from Overton, auto-detects the PDF URL column, and
#' downloads each unique PDF to a local output directory. Already-downloaded
#' files are skipped so the function can be safely re-run after interruptions.
#'
#' @param input_path `character(1)`. Path to the Overton CSV export file.
#' @param out_dir `character(1)`. Path to the directory where PDFs will be
#'   saved. Created automatically (including parents) if it does not exist.
#' @param sleep_sec `numeric(1)`. Seconds to wait between requests. Increase
#'   this to be more respectful to remote servers. Default is `0.1`.
#' @param max_tries `integer(1)`. Maximum number of retry attempts per URL.
#'   Default is `4`.
#'
#' @return A [tibble][tibble::tibble] with one row per unique PDF URL and the
#'   following columns:
#'   \describe{
#'     \item{doc_id}{Document identifier (from `document_id`, `overton_id`,
#'       or row number).}
#'     \item{pdf_url}{The URL that was (or was attempted to be) downloaded.}
#'     \item{file}{Full path to the local file.}
#'     \item{status}{HTTP status code, or `NA` on a connection error.}
#'     \item{ok}{`TRUE` if the file downloaded successfully or already
#'       existed; `FALSE` on failure.}
#'     \item{note}{Human-readable summary of the outcome.}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' results <- download_overton_pdfs(
#'   input_path = "path/to/overton_export.csv",
#'   out_dir    = "path/to/output_directory"
#' )
#'
#' # Inspect failures
#' failed <- results[!results$ok, ]
#' print(failed)
#' }
#'
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @importFrom dplyr transmute filter distinct bind_cols count n
#' @importFrom stringr str_detect
#' @importFrom purrr pmap_dfr
#' @importFrom httr2 request req_user_agent req_timeout req_retry req_perform
#'   resp_status resp_header
#' @importFrom tibble tibble
download_overton_pdfs <- function(input_path,
                                  out_dir,
                                  sleep_sec = 0.1,
                                  max_tries = 4L) {

  # ---- Validate inputs -------------------------------------------------------
  if (!file.exists(input_path)) {
    stop("'input_path' does not exist: ", input_path, call. = FALSE)
  }

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # ---- Read & clean ----------------------------------------------------------
  df <- readr::read_csv(input_path, show_col_types = FALSE) |>
    janitor::clean_names()

  # ---- Auto-detect columns ---------------------------------------------------
  pdf_col   <- .detect_pdf_col(df)
  id_col    <- .first_match(names(df), c("document_id", "overton_id", "id"))
  title_col <- .first_match(names(df), c("title", "document_title", "name"))

  # ---- Build download table --------------------------------------------------
  dl <- df |>
    dplyr::transmute(
      doc_id  = if (!is.na(id_col))    as.character(.data[[id_col]])
      else as.character(seq_len(dplyr::n())),
      title   = if (!is.na(title_col)) as.character(.data[[title_col]])
      else NA_character_,
      pdf_url = as.character(.data[[pdf_col]])
    ) |>
    dplyr::filter(!is.na(pdf_url), pdf_url != "") |>
    dplyr::distinct(pdf_url, .keep_all = TRUE)

  message("Found ", nrow(dl), " unique PDF URLs to process.")

  # ---- Download loop ---------------------------------------------------------
  results <- purrr::pmap_dfr(
    list(dl$doc_id, dl$title, dl$pdf_url),
    function(doc_id, title, pdf_url) {

      fname <- paste0(doc_id, "__", .safe_slug(title), ".pdf")
      dest  <- file.path(out_dir, fname)

      if (file.exists(dest) && file.size(dest) > 0) {
        return(tibble::tibble(
          doc_id  = doc_id,  pdf_url = pdf_url, file = dest,
          status  = 200L,    ok = TRUE,          note = "skipped (exists)"
        ))
      }

      r <- .download_one(pdf_url, dest_path = dest,
                         sleep_sec = sleep_sec, max_tries = max_tries)
      dplyr::bind_cols(
        tibble::tibble(doc_id = doc_id, pdf_url = pdf_url, file = dest),
        r
      )
    }
  )

  # ---- Summary ---------------------------------------------------------------
  message("\nDownload summary:")
  print(dplyr::count(results, ok, note))

  n_failed <- sum(!results$ok)
  if (n_failed > 0) {
    message(
      "\n", n_failed, " download(s) failed. ",
      "Filter with `result[!result$ok, ]` to inspect them."
    )
  }

  invisible(results)
}
