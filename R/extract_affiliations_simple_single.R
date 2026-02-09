#' Extract simplified affiliation rows from a single Lens.org JSONL/JSONL.GZ export
#'
#' Internal helper used by [extract_affiliations_publications_lens_single()] to parse one `.jsonl`
#' or `.jsonl.gz` file and return a row-wise affiliation table.
#'
#' The function reads the file line-by-line, parses each line as JSON (dropping
#' malformed records), iterates over `authors` and their `affiliations`, and
#' returns one row per author–affiliation combination. Where available, it also
#' extracts common organization identifiers (ROR/GRID/OpenAlex/FundRef/Wikidata).
#'
#' @param file_path A path to a single `.jsonl` or `.jsonl.gz` file.
#'
#' @return A data frame with columns (when present in the source):
#' \describe{
#'   \item{lens_id}{Character. Lens publication identifier.}
#'   \item{year}{Integer. Publication year if available, otherwise `NA`.}
#'   \item{author_name}{Character. Author display name (constructed when missing).}
#'   \item{affiliation_name}{Character. Affiliation name if present.}
#'   \item{affiliation_name_original}{Character. Original affiliation name if present.}
#'   \item{city}{Character. Affiliation city if present.}
#'   \item{state}{Character. Affiliation state/region if present.}
#'   \item{country}{Character. Affiliation country if present.}
#'   \item{country_code}{Character. Affiliation country code if present.}
#'   \item{ror_id}{Character. ROR identifier if present.}
#'   \item{grid_id}{Character. GRID identifier if present.}
#'   \item{openalex_id}{Character. OpenAlex identifier if present.}
#'   \item{fundref_id}{Character. FundRef identifier if present.}
#'   \item{wikidata_id}{Character. Wikidata identifier if present.}
#' }
#'
#' @details
#' This is an internal function and is not exported.
#'
#' The implementation uses the infix operator `%||%` from **rlang** to provide
#' fallbacks for missing values.
#'
#' @keywords internal
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom rlang %||%
#'
#' @examples
#' \dontrun{
#' # Typically not called directly by users:
#' df <- extract_affiliations_simple_single("data/lens_publications.jsonl.gz")
#' }
extract_affiliations_simple_single <- function(file_path) {

  con <- if (grepl("\\.gz$", file_path)) {
    gzfile(file_path, "rt")
  } else {
    file(file_path, "rt")
  }

  lines <- readLines(con, warn = FALSE)
  close(con)

  all_affiliations <- list()

  for (line in lines) {
    record <- tryCatch(fromJSON(line, flatten = FALSE), error = function(e) NULL)
    if (is.null(record)) next

    lens_id <- record$lens_id %||% NA
    year <- record$year_published %||%
      (if (!is.null(record$date_published)) as.integer(substr(record$date_published, 1, 4)) else NA)

    if (!is.null(record$authors)) {
      for (author in record$authors) {
        author_name <- author$display_name %||%
          paste(author$first_name %||% "", author$last_name %||% "") %||% NA

        if (!is.null(author$affiliations) && length(author$affiliations) > 0) {
          for (affil in author$affiliations) {
            # Extract IDs from the nested ids data frame
            ror_id <- NA
            grid_id <- NA
            openalex_id <- NA
            fundref_id <- NA
            wikidata_id <- NA

            if (!is.null(affil$ids)) {
              ids_df <- if (is.data.frame(affil$ids)) {
                affil$ids
              } else if (is.list(affil$ids)) {
                tryCatch(as.data.frame(affil$ids), error = function(e) NULL)
              } else {
                NULL
              }

              if (!is.null(ids_df) && nrow(ids_df) > 0) {
                # Extract each ID type
                if ("type" %in% names(ids_df) && "value" %in% names(ids_df)) {
                  ror_id <- ids_df$value[ids_df$type == "ror"][1] %||% NA
                  grid_id <- ids_df$value[ids_df$type == "grid"][1] %||% NA
                  openalex_id <- ids_df$value[ids_df$type == "openalex"][1] %||% NA
                  fundref_id <- ids_df$value[ids_df$type == "fundref"][1] %||% NA
                  wikidata_id <- ids_df$value[ids_df$type == "wikidata"][1] %||% NA
                }
              }
            }

            all_affiliations[[length(all_affiliations) + 1]] <- data.frame(
              lens_id = lens_id,
              year = year,
              author_name = author_name,
              affiliation_name = affil$name %||% NA,
              affiliation_name_original = affil$name_original %||% NA,
              city = affil$city %||% NA,
              state = affil$state %||% NA,
              country = affil$country %||% NA,
              country_code = affil$country_code %||% NA,
              ror_id = ror_id,
              grid_id = grid_id,
              openalex_id = openalex_id,
              fundref_id = fundref_id,
              wikidata_id = wikidata_id,
              stringsAsFactors = FALSE
            )
          }
        } else {
          # Author with no affiliation
          all_affiliations[[length(all_affiliations) + 1]] <- data.frame(
            lens_id = lens_id,
            year = year,
            author_name = author_name,
            affiliation_name = NA,
            affiliation_name_original = NA,
            city = NA,
            state = NA,
            country = NA,
            country_code = NA,
            ror_id = NA,
            grid_id = NA,
            openalex_id = NA,
            fundref_id = NA,
            wikidata_id = NA,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(all_affiliations) == 0) {
    return(data.frame())
  }

  bind_rows(all_affiliations)
}
