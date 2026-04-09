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

    mutate(
      affil_clean = affiliation_full %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all("[[:punct:]]", " ") %>%
        stringr::str_squish(),

      affil_commaclean = affiliation_full %>%
        stringr::str_replace_all("[，、؛;]", ",") %>%
        stringr::str_replace_all("\\s*,\\s*", ",") %>%
        stringr::str_replace_all("\\s+", " ") %>%
        stringr::str_squish(),

      has_comma             = stringr::str_detect(affil_commaclean, ","),
      first_chunk           = stringr::str_trim(stringr::str_replace(affil_commaclean, ",.*$", "")),
      remainder_after_first = stringr::str_trim(stringr::str_replace(affil_commaclean, "^[^,]*,\\s*", "")),

      # --------------------------------------------------
      # Department-like detector (incl. State Key Laboratory)
      # --------------------------------------------------
      dept_like_first = stringr::str_detect(
        stringr::str_to_lower(first_chunk),
        stringr::regex(
          "^(department(\\s+(of|for))?
      |dept(\\.| )?(of|for)?
      |school of
      |faculty of
      |college of
      |division of
      |unit of
      |chair of
      |laborator(y|io|ie) (of|de)
      |clinic(al)? (of|de)
      |state key laborator(y|y of|y for|y in)
      )\\b",
          ignore_case = TRUE
        )
      ),

      # --------------------------------------------------
      # 1) UNIVERSITY: detect & extract the comma-bounded segment containing it
      # --------------------------------------------------
      uni_segment = stringr::str_extract(
        affil_commaclean,
        stringr::regex(
          "(^|,)\\s*[^,]*\\b(university|univ\\b|uni\\b|universit\\p{L}*)\\b[^,]*",
          ignore_case = TRUE
        )
      ),
      university_chunk = dplyr::if_else(
        is.na(uni_segment),
        NA_character_,
        stringr::str_replace(stringr::str_trim(uni_segment), "^,\\s*", "")
      ),

      # --------------------------------------------------
      # 2) MAJOR NON-UNIVERSITY INSTITUTIONS
      # --------------------------------------------------
      major_inst_pattern = paste(c(
        # Major research systems
        "max\\s+planck",
        "helmholtz",
        "fraunhofer",
        "\\bcnrs\\b",
        "\\bnist\\b",
        "\\briken\\b",
        "chinese\\s+academy\\s+of\\s+sciences",
        "polish\\s+academy\\s+of\\s+sciences",
        # National labs
        "national\\s+laboratory",
        "national\\s+lab\\b",
        "sandia",
        "lincoln\\s+lab",
        "argonne",
        "oak\\s+ridge",
        "bell\\s+labs",
        "bell\\s+laborator",
        # Semiconductor-specific centres
        "\\bimec\\b",
        "\\bleti\\b",
        "\\bcea\\b",
        "\\bimtek\\b",
        "interuniversity\\s+microelectronics",
        # Research centres/institutes
        "research\\s+institute",
        "research\\s+cent(er|re)",
        "research\\s+lab",
        "institute\\s+of\\b",
        "state\\s+grid\\s+electric\\s+power\\s+research\\s+institute",
        # Semiconductor & materials keywords
        "semiconductor",
        "microelectron",
        "nanoelectron",
        "integrated\\s+circuit",
        "nanotech",
        "nanoscience",
        "photonic",
        "optoelectron",
        "thin\\s+film",
        # Elite institutions without 'university'
        "\\bmit\\b",
        "\\bcaltech\\b",
        "\\bepfl\\b",
        "eth\\s+zurich",
        "\\bkth\\b",
        "imperial\\s+college",
        "\\btum\\b",
        "\\bucla\\b",
        "\\bstanford\\b",
        "massachusetts\\s+institute\\s+of\\s+technology",
        "california\\s+institute\\s+of\\s+technology",
        "georgia\\s+institute\\s+of\\s+technology",
        "indian\\s+institute\\s+of\\s+technology",
        "nanyang\\s+technological",
        "delft\\s+university\\s+of\\s+technology",
        "eindhoven\\s+university\\s+of\\s+technology",
        # Polytechnics / Hochschulen
        "polytechnic",
        "polytechnique",
        "hochschule",
        "hogeschool",
        "fachhochschule"
      ), collapse = "|"),

      inst_segment = stringr::str_extract(
        affil_commaclean,
        stringr::regex(
          paste0("(^|,)\\s*[^,]*\\b(", major_inst_pattern, ")\\b[^,]*"),
          ignore_case = TRUE
        )
      ),
      major_inst_chunk = dplyr::if_else(
        is.na(inst_segment),
        NA_character_,
        stringr::str_replace(stringr::str_trim(inst_segment), "^,\\s*", "")
      ),

      # --------------------------------------------------
      # Final decision (priority: university > major institute > dept fallback > original)
      # --------------------------------------------------
      base_affil = dplyr::case_when(
        !is.na(university_chunk) ~ university_chunk,
        !is.na(major_inst_chunk) ~ major_inst_chunk,
        dept_like_first & has_comma ~ remainder_after_first,
        TRUE ~ affil_commaclean
      ),

      has_comma_base = stringr::str_detect(base_affil, ","),
      affiliation_name = dplyr::if_else(
        has_comma_base,
        stringr::str_trim(stringr::str_replace(base_affil, ",.*$", "")),
        stringr::str_trim(base_affil)
      ),

      country = dplyr::if_else(
        has_comma,
        str_trim(str_replace(affiliation_full, ".*,(.*)$", "\\1")),
        NA_character_
      ),

      # --------------------------------------------------
      # Match type for auditing
      # --------------------------------------------------
      match_type = dplyr::case_when(
        !is.na(university_chunk) ~ "university",
        !is.na(major_inst_chunk) ~ "institute_or_semicon",
        TRUE                     ~ NA_character_
      )
    ) |>

    # Keep only matched rows and relevant columns
    filter(!is.na(match_type)) |>
    select(eid,
           affiliation_name,
           country)
}
