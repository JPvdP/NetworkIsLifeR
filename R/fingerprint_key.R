#' Compute the fingerprint key for a single string
#'
#' Implements the fingerprint keying algorithm from OpenRefine. The string is
#' trimmed, lowercased, stripped of diacritics and punctuation, split into
#' whitespace-delimited tokens, deduplicated, sorted alphabetically, and
#' rejoined with a single space. Two strings that differ only in case,
#' diacritics, punctuation, or word order will produce the same key and
#' therefore belong to the same cluster.
#'
#' When `company = TRUE`, two additional steps are applied to the normalised
#' string *before* fingerprinting, enabling company-name harmonisation across
#' different legal forms and naming conventions:
#'
#' 1. **Legal suffix stripping** — entity-type suffixes (e.g. `"BV"`, `"GmbH"`,
#'    `"Inc."`, `"Ltd"`, `"SARL"`) are removed. The list covers all EU-27
#'    member states, the United States, and China. Matching is
#'    case-insensitive and punctuation-agnostic (`"B.V."`, `"bv"`, and `"BV"`
#'    are all recognised). Stacked suffixes (e.g. `"BV Holding"`) are removed
#'    iteratively.
#'
#' 2. **Term harmonisation** — common long-form descriptors are contracted to
#'    their standard abbreviation so that, for example, `"Philips Technology"`
#'    and `"Philips Tech"` share the same key. The contractions applied are:
#'    \tabular{ll}{
#'      **Long form** \tab **Abbreviation** \cr
#'      technology / technologies / technological \tab tech \cr
#'      management / mgmt \tab mgt \cr
#'      industry / industries / industrial \tab ind \cr
#'      company / companies \tab co \cr
#'      international \tab intl \cr
#'      laboratory / laboratories / labs \tab lab \cr
#'      manufacturing / manufacturer \tab mfg \cr
#'      development / developer \tab dev \cr
#'    }
#'    All matching is whole-word and case-insensitive, so partial matches
#'    inside longer words (e.g. `"mismanagement"`) are never affected.
#'
#' The raw value passed in is **never modified**; only the internal clustering
#' key is affected.
#'
#' @param s A single character string. `NA` returns `NA_character_`.
#' @param company Logical. If `TRUE`, legal entity suffixes are stripped and
#'   common term variants are contracted before the fingerprint key is
#'   computed. Defaults to `FALSE`.
#'
#' @return A single character string containing the fingerprint key, or
#'   `NA_character_` if `s` is `NA`.
#'
#' @seealso [fingerprint_key_vec()] for a vectorised version,
#'   [fingerprint_clean()] to apply clustering to a data frame column.
#'
#' @references
#'   OpenRefine FingerprintKeyer source:
#'   \url{https://github.com/OpenRefine/OpenRefine/blob/master/modules/core/src/main/java/com/google/refine/clustering/binning/FingerprintKeyer.java}
#'
#' @examples
#' # --- Standard usage ---------------------------------------------------------
#' fingerprint_key("Philips")           # "philips"
#' fingerprint_key("PHILIPS")           # "philips"
#' fingerprint_key("Stra\u00DFe")       # "strasse"
#' fingerprint_key("Hello, World!")     # "hello world"
#' fingerprint_key(NA_character_)       # NA
#'
#' # --- Company mode: legal suffix stripping -----------------------------------
#' fingerprint_key("Philips BV",    company = TRUE)  # "philips"
#' fingerprint_key("Philips GmbH",  company = TRUE)  # "philips"
#' fingerprint_key("Philips B.V.",  company = TRUE)  # "philips"  (punct variant)
#' fingerprint_key("Philips",       company = TRUE)  # "philips"  (same key)
#'
#' # --- Company mode: term harmonisation ---------------------------------------
#' fingerprint_key("Philips Technology", company = TRUE)  # "philips tech"
#' fingerprint_key("Philips Tech",       company = TRUE)  # "philips tech"
#' fingerprint_key("Siemens Management Solutions BV", company = TRUE)
#' # -> "mgt siemens solutions"  (BV stripped, management -> mgt)
#'
#' @export
fingerprint_key <- function(s, company = FALSE) {
  if (is.na(s)) return(NA_character_)
  if (!is.character(s) || length(s) != 1L) {
    stop("`s` must be a single character string.")
  }

  s <- .fp_normalize(s)

  if (isTRUE(company)) {
    s <- .fp_company_pipeline(s)
  }

  tokens <- unlist(strsplit(s, "\\s+", perl = TRUE))
  tokens <- tokens[nchar(tokens) > 0L]
  tokens <- sort(unique(tokens))
  paste(tokens, collapse = " ")
}


#' Compute fingerprint keys for a character vector
#'
#' A vectorised wrapper around [fingerprint_key()] that returns a character
#' vector of fingerprint keys the same length as the input.
#'
#' @param x A character vector.
#' @param company Logical. Passed to [fingerprint_key()]. If `TRUE`, legal
#'   entity suffixes are stripped and common term variants are contracted before
#'   fingerprinting. Defaults to `FALSE`.
#'
#' @return A character vector of fingerprint keys the same length as `x`,
#'   with `NA` values preserved.
#'
#' @seealso [fingerprint_key()] for full documentation of the `company` pipeline,
#'   [fingerprint_clean()] to apply clustering to a data frame column.
#'
#' @examples
#' x <- c("Philips BV", "Philips GmbH", "PHILIPS",
#'         "Sony Corp.", "Sony", "SONY CORPORATION", NA)
#'
#' # Standard mode — suffixes remain in the key
#' fingerprint_key_vec(x)
#'
#' # Company mode — all Philips / Sony variants share one key each
#' fingerprint_key_vec(x, company = TRUE)
#' # [1] "philips" "philips" "philips" "sony" "sony" "sony" NA
#'
#' x2 <- c("Philips Technology BV", "Philips Tech", "Philips Technologies")
#' fingerprint_key_vec(x2, company = TRUE)
#' # [1] "philips tech" "philips tech" "philips tech"
#'
#' @export
fingerprint_key_vec <- function(x, company = FALSE) {
  vapply(x, fingerprint_key, character(1L), company = company, USE.NAMES = FALSE)
}
