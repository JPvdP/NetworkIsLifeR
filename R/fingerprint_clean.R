#' Clean a character column using fingerprint clustering
#'
#' Clusters values in a data frame column using fingerprint keying (see
#' [fingerprint_key()]) and adds a new column containing the canonical
#' reference name for each cluster. The canonical name is the **most frequent**
#' raw value within the cluster; ties are broken alphabetically (the
#' lexicographically first value among the tied values is chosen), ensuring
#' deterministic output.
#'
#' The fingerprinting pipeline treats two strings as equivalent when they differ
#' only in case, diacritics, punctuation, or word order.
#'
#' When `company = TRUE`, two additional steps are applied to the fingerprint
#' key *before* clustering (the raw values in your data are never modified):
#'
#' 1. **Legal suffix stripping** — entity-type suffixes are removed so that
#'    `"Philips BV"`, `"Philips GmbH"`, and `"Philips"` all map to the same
#'    cluster. The suffix list covers all EU-27 member states, the United
#'    States, and China. Matching is case-insensitive and handles punctuation
#'    variants automatically (`"B.V."`, `"bv"`, `"BV"` are all caught).
#'    Stacked suffixes (e.g. `"BV Holding"`) are removed iteratively.
#'
#' 2. **Term harmonisation** — common long-form descriptors are contracted to a
#'    standard abbreviation so that `"Philips Technology"` and `"Philips Tech"`
#'    cluster together. The contractions applied are:
#'    \tabular{ll}{
#'      **Long form(s)** \tab **Abbreviation** \cr
#'      technology / technologies / technological \tab tech \cr
#'      management / mgmt \tab mgt \cr
#'      industry / industries / industrial \tab ind \cr
#'      company / companies \tab co \cr
#'      international \tab intl \cr
#'      laboratory / laboratories / labs \tab lab \cr
#'      manufacturing / manufacturer \tab mfg \cr
#'      development / developer \tab dev \cr
#'    }
#'    All matching is whole-word and case-insensitive.
#'
#' @param df A data frame.
#' @param col The column to cluster. Can be passed unquoted (`brand`) or as a
#'   string (`"brand"`).
#' @param clean_col A single string giving the name of the new column that will
#'   hold the canonical reference name. Defaults to `"clean"`.
#' @param company Logical. If `TRUE`, legal entity suffixes are stripped and
#'   common term variants are contracted before clustering, enabling
#'   company-name harmonisation across different legal forms and naming
#'   conventions. The original values are preserved unchanged. Defaults to
#'   `FALSE`.
#'
#' @return The original data frame with one additional column named `clean_col`.
#'   Row order and all existing columns are preserved. `NA` values in `col`
#'   produce `NA` in `clean_col`.
#'
#' @seealso [fingerprint_key()] for the underlying keying algorithm and full
#'   details of the `company` pipeline,
#'   [fingerprint_key_vec()] to inspect the raw fingerprint keys directly.
#'
#' @references
#'   OpenRefine FingerprintKeyer source:
#'   \url{https://github.com/OpenRefine/OpenRefine/blob/master/modules/core/src/main/java/com/google/refine/clustering/binning/FingerprintKeyer.java}
#'
#' @examples
#' df <- data.frame(
#'   name = c(
#'     "Philips BV", "Philips GmbH", "PHILIPS", "Philips B.V.",
#'     "Philips Technology BV", "Philips Tech",
#'     "Sony Corp.", "Sony Corporation", "SONY",
#'     NA
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Standard mode — legal suffixes and term variants affect clustering
#' fingerprint_clean(df, name)
#'
#' # Company mode — suffixes stripped and terms harmonised before clustering.
#' # "Philips BV", "Philips GmbH", "PHILIPS" all map to the same cluster;
#' # "Philips Technology BV" and "Philips Tech" also cluster together
#' # (both key to "philips tech" after stripping and harmonisation).
#' fingerprint_clean(df, name, company = TRUE)
#' #>                    name        clean
#' #> 1            Philips BV   Philips BV
#' #> 2          Philips GmbH   Philips BV
#' #> 3               PHILIPS   Philips BV
#' #> 4          Philips B.V.   Philips BV
#' #> 5  Philips Technology BV  Philips Technology BV
#' #> 6          Philips Tech   Philips Technology BV
#' #> 7            Sony Corp.     Sony Corp.
#' #> 8      Sony Corporation     Sony Corp.
#' #> 9                  SONY     Sony Corp.
#' #> 10                 <NA>         <NA>
#'
#' # Custom output column name
#' fingerprint_clean(df, name, clean_col = "name_clean", company = TRUE)
#'
#' # Column name passed as a string (useful when called programmatically)
#' col <- "name"
#' fingerprint_clean(df, col = col, company = TRUE)
#'
#' @export
fingerprint_clean <- function(df, col, clean_col = "clean", company = FALSE) {
  if (!is.data.frame(df)) stop("`df` must be a data frame.")

  # Accept both unquoted (NSE) and pre-quoted column names.
  col_name <- tryCatch(
    as.character(substitute(col)),
    error = function(e) col
  )
  # If the user passed a variable whose *value* is the column name, resolve it.
  if (!col_name %in% names(df) && exists(col_name, inherits = TRUE)) {
    col_name <- get(col_name)
  }
  if (!col_name %in% names(df)) {
    stop(sprintf("Column '%s' not found in `df`.", col_name))
  }
  if (!is.character(clean_col) || length(clean_col) != 1L || !nzchar(clean_col)) {
    stop("`clean_col` must be a non-empty character string.")
  }
  if (!is.logical(company) || length(company) != 1L || is.na(company)) {
    stop("`company` must be TRUE or FALSE.")
  }

  x    <- df[[col_name]]
  keys <- fingerprint_key_vec(x, company = company)

  lookup          <- .fp_canonical_lookup(x, keys)
  df[[clean_col]] <- lookup[keys]

  df
}
