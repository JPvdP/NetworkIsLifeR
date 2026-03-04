# Internal helpers for download_overton_pdfs() --------------------------------
# These are not exported; document with @noRd to suppress man/ page generation.

#' @noRd
.detect_pdf_col <- function(df) {
  nms     <- names(df)
  matches <- nms[stringr::str_detect(nms, "document") &
                   stringr::str_detect(nms, "url")]

  if (length(matches) == 0) {
    stop(
      "Could not auto-detect a PDF URL column. ",
      "Column names found: ", paste(nms, collapse = ", "), ". ",
      "Rename your PDF URL column to contain both 'document' and 'url'.",
      call. = FALSE
    )
  }
  matches[[1]]
}

#' @noRd
.first_match <- function(haystack, needles) {
  hit <- intersect(haystack, needles)
  if (length(hit)) hit[[1]] else NA_character_
}

#' @noRd
.safe_slug <- function(x, max_len = 120L) {
  x <- ifelse(is.na(x) | x == "", "document", x)
  x |>
    stringr::str_squish() |>
    stringr::str_replace_all("[^A-Za-z0-9._-]+", "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_replace_all("^_+|_+$", "") |>
    substr(1L, max_len)
}

#' @noRd
.download_one <- function(url, dest_path, sleep_sec = 0.1, max_tries = 4L) {
  Sys.sleep(sleep_sec)

  req <- httr2::request(url) |>
    httr2::req_user_agent("R (Overton PDF downloader; research use)") |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = max_tries, backoff = ~ 1 + .x)

  resp <- tryCatch(
    httr2::req_perform(req, path = dest_path),
    error = function(e) e
  )

  if (inherits(resp, "error")) {
    if (file.exists(dest_path)) file.remove(dest_path)
    return(tibble::tibble(
      status = NA_integer_, ok = FALSE,
      note   = paste("request error:", resp$message)
    ))
  }

  st <- httr2::resp_status(resp)
  ct <- httr2::resp_header(resp, "content-type") %||% ""

  if (st < 200L || st >= 300L) {
    if (file.exists(dest_path)) file.remove(dest_path)
    return(tibble::tibble(status = st, ok = FALSE, note = paste("HTTP", st)))
  }

  if (!stringr::str_detect(tolower(ct), "pdf")) {
    return(tibble::tibble(
      status = st, ok = TRUE,
      note   = paste("downloaded; content-type:", ct)
    ))
  }

  tibble::tibble(status = st, ok = TRUE, note = "downloaded")
}

#' @noRd
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
