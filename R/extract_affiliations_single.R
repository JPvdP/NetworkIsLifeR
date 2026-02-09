#' Extract publication affiliations from a single Lens.org JSONL/JSONL.GZ export
#'
#' Internal helper used by [extract_affiliations_publications_lens()] to parse one
#' `.jsonl` or `.jsonl.gz` file and return a row-wise affiliation table.
#'
#' The function reads the file line-by-line, parses each line as JSON (dropping
#' malformed records), iterates through `authors` and their `affiliations`, and
#' returns one row per author–affiliation combination (with identifier columns
#' such as ROR/GRID/OpenAlex/FundRef/Wikidata when present).
#'
#' @param file_path A path to a single `.jsonl` or `.jsonl.gz` file.
#' @param expand_authors Logical. Currently retained for API compatibility with
#'   [extract_affiliations_publications_lens()]. In this implementation, rows are
#'   generated at the author/affiliation level regardless; the argument may be
#'   used in future revisions to control aggregation.
#'
#' @return A data frame with (at minimum) the following columns:
#' \describe{
#'   \item{lens_id}{Character. Lens publication identifier.}
#'   \item{year}{Integer. Publication year if available, otherwise `NA`.}
#'   \item{author_name}{Character. Author display name (constructed when missing).}
#'   \item{affiliation_name}{Character. Normalized affiliation name if present.}
#'   \item{affiliation_name_original}{Character. Original affiliation name if present.}
#'   \item{affiliation_city}{Character. City of the affiliation if present.}
#'   \item{affiliation_state}{Character. State/region of the affiliation if present.}
#'   \item{affiliation_country}{Character. Country name if present.}
#'   \item{affiliation_country_code}{Character. Country code if present.}
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
#' df <- extract_affiliations_single("data/lens_publications.jsonl.gz")
#' }
extract_affiliations_single <- function(file_path, expand_authors = TRUE) {

  # Read the JSONL file
  con <- if (grepl("\\.gz$", file_path)) {
    gzfile(file_path, "rt")
  } else {
    file(file_path, "rt")
  }

  lines <- readLines(con, warn = FALSE)
  close(con)

  # Parse each line as JSON (flatten = FALSE to preserve nested structure)
  records <- lapply(lines, function(x) {
    tryCatch(
      fromJSON(x, flatten = FALSE),
      error = function(e) NULL
    )
  })

  # Remove NULL entries
  records <- records[!sapply(records, is.null)]

  # Extract affiliations using a simpler approach
  all_rows <- list()

  for (i in seq_along(records)) {
    record <- records[[i]]

    # Extract basic info
    lens_id <- record$lens_id %||% NA
    year <- record$year_published %||%
      (if (!is.null(record$date_published)) as.integer(substr(record$date_published, 1, 4)) else NA)

    # Check if authors exist
    if (is.null(record$authors) || length(record$authors) == 0) {
      next
    }

    # Process each author
    authors_list <- if (is.data.frame(record$authors)) {
      # Convert data frame to list of rows
      lapply(seq_len(nrow(record$authors)), function(j) as.list(record$authors[j, ]))
    } else if (is.list(record$authors)) {
      record$authors
    } else {
      next
    }

    for (author in authors_list) {
      tryCatch({
        # Extract author name
        author_name <- author$display_name %||%
          paste(author$first_name %||% "", author$last_name %||% "") %||%
          NA

        # Check if author has affiliations
        if (!is.null(author$affiliations) && length(author$affiliations) > 0) {

          # Handle affiliations (can be list or data frame)
          affils <- if (is.data.frame(author$affiliations)) {
            lapply(seq_len(nrow(author$affiliations)), function(k) as.list(author$affiliations[k, ]))
          } else if (is.list(author$affiliations)) {
            author$affiliations
          } else {
            NULL
          }

          # Only process if affils is not NULL and has content
          if (!is.null(affils) && length(affils) > 0) {
            # Create one row per affiliation if expand_authors is TRUE
            for (affil in affils) {
              # Skip if affil is NULL or empty
              if (is.null(affil) || (is.list(affil) && length(affil) == 0)) {
                next
              }

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
                  if ("type" %in% names(ids_df) && "value" %in% names(ids_df)) {
                    ror_id <- ids_df$value[ids_df$type == "ror"][1] %||% NA
                    grid_id <- ids_df$value[ids_df$type == "grid"][1] %||% NA
                    openalex_id <- ids_df$value[ids_df$type == "openalex"][1] %||% NA
                    fundref_id <- ids_df$value[ids_df$type == "fundref"][1] %||% NA
                    wikidata_id <- ids_df$value[ids_df$type == "wikidata"][1] %||% NA
                  }
                }
              }

              all_rows[[length(all_rows) + 1]] <- data.frame(
                lens_id = lens_id,
                year = year,
                author_name = author_name,
                affiliation_name = affil$name %||% NA,
                affiliation_name_original = affil$name_original %||% NA,
                affiliation_city = affil$city %||% NA,
                affiliation_state = affil$state %||% NA,
                affiliation_country = affil$country %||% NA,
                affiliation_country_code = affil$country_code %||% NA,
                ror_id = ror_id,
                grid_id = grid_id,
                openalex_id = openalex_id,
                fundref_id = fundref_id,
                wikidata_id = wikidata_id,
                stringsAsFactors = FALSE
              )
            }
          } else {
            # Affiliations field exists but is empty
            all_rows[[length(all_rows) + 1]] <- data.frame(
              lens_id = lens_id,
              year = year,
              author_name = author_name,
              affiliation_name = NA,
              affiliation_name_original = NA,
              affiliation_city = NA,
              affiliation_state = NA,
              affiliation_country = NA,
              affiliation_country_code = NA,
              ror_id = NA,
              grid_id = NA,
              openalex_id = NA,
              fundref_id = NA,
              wikidata_id = NA,
              stringsAsFactors = FALSE
            )
          }
        } else {
          # Author with no affiliation
          all_rows[[length(all_rows) + 1]] <- data.frame(
            lens_id = lens_id,
            year = year,
            author_name = author_name,
            affiliation_name = NA,
            affiliation_name_original = NA,
            affiliation_city = NA,
            affiliation_state = NA,
            affiliation_country = NA,
            affiliation_country_code = NA,
            ror_id = NA,
            grid_id = NA,
            openalex_id = NA,
            fundref_id = NA,
            wikidata_id = NA,
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        # Skip problematic authors silently
        warning("Skipping author in record ", lens_id, ": ", e$message)
      })
    }
  }

  if (length(all_rows) == 0) {
    warning("No affiliation data found in file: ", basename(file_path))
    return(data.frame(
      lens_id = character(),
      year = integer(),
      author_name = character(),
      affiliation_name = character(),
      affiliation_name_original = character(),
      affiliation_city = character(),
      affiliation_state = character(),
      affiliation_country = character(),
      affiliation_country_code = character(),
      ror_id = character(),
      grid_id = character(),
      openalex_id = character(),
      fundref_id = character(),
      wikidata_id = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Combine all rows
  result <- bind_rows(all_rows)

  return(as.data.frame(result))
}
