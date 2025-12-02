#' Split Scopus affiliations to one row per institution and extract country/name
#'
#' This function processes a Scopus export by splitting the affiliation field
#' (typically a semicolon-separated string containing multiple institutions) into
#' one row per affiliation. For each affiliation, it extracts:
#'
#' * the full affiliation string,
#' * the affiliation name (text before the first comma),
#' * the country (text after the last comma).
#'
#' This is useful for building collaboration networks, country-level statistics,
#' and affiliation-level bibliometric analysis.
#'
#' @param df A data frame containing (at least) an EID column and an affiliation column.
#' @param eid_col Character. Name of the column containing the Scopus EID.
#' @param affil_col Character. Name of the column containing the raw affiliation text.
#'
#' @return A tibble with one row per affiliation, containing:
#' \itemize{
#'   \item \code{eid} – the Scopus EID.
#'   \item \code{affiliation_full} – the full affiliation string.
#'   \item \code{affiliation_name} – the text before the first comma.
#'   \item \code{country} – the text after the last comma.
#' }
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   EID = "2-s2.0-12345",
#'   Affiliations = "Utrecht University, Utrecht, Netherlands;
#'                   University of Oxford, Oxford, United Kingdom"
#' )
#'
#' split_scopus_affiliations(data)
#' }
#'
#' @seealso \code{\link[tidyr]{separate_rows}}, \code{\link[stringr]{str_trim}}
#'
#' @export
split_scopus_affiliations <- function(df,
                                      eid_col  = "EID",
                                      affil_col = "Affiliations") {
  # Needs dplyr, tidyr, stringr
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE) ||
      !requireNamespace("stringr", quietly = TRUE)) {
    stop("Please install 'dplyr', 'tidyr', and 'stringr' first.")
  }

  library(dplyr)
  library(tidyr)
  library(stringr)

  # Work on a copy with standard names
  df |>
    dplyr::select(
      eid = dplyr::all_of(eid_col),
      affiliations_raw = dplyr::all_of(affil_col)
    ) |>

    # One row per affiliation (split on ;)
    tidyr::separate_rows(affiliations_raw, sep = ";\\s*") |>

    # Clean whitespace and trailing period
    mutate(
      affiliation_full = str_trim(affiliations_raw),
      affiliation_full = str_remove(affiliation_full, "\\.$")
    ) |>

    # Extract name (before first comma) and country (after last comma)
    mutate(
      has_comma = str_detect(affiliation_full, ","),

      affiliation_name = dplyr::if_else(
        has_comma,
        str_trim(str_replace(affiliation_full, ",.*$", "")),  # before first comma
        str_trim(affiliation_full)
      ),

      country = dplyr::if_else(
        has_comma,
        str_trim(str_replace(affiliation_full, ".*,(.*)$", "\\1")),  # after last comma (greedy)
        NA_character_
      )
    ) |>
    select(eid,
           affiliation_full,
           affiliation_name,
           country)
}
