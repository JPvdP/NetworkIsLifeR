#' Assign Most Frequent Country and Address to Each Assignee
#'
#' This function takes a patent database with multiple rows per assignee and
#' assigns the most frequent country (residence) and address to each unique
#' assignee name. This is useful for consolidating assignee information when
#' the same entity appears with different locations across multiple patents.
#'
#' @param df A data frame containing patent assignee information.
#' @param name_col Character string specifying the column name containing
#'   assignee names. Default is "name".
#' @param residence_col Character string specifying the column name containing
#'   country/residence information. Default is "residence".
#' @param address_col Character string specifying the column name containing
#'   address information. Default is "address".
#'
#' @return A data frame with one row per unique assignee name containing:
#'   \itemize{
#'     \item Column with assignee names (using the name specified in \code{name_col})
#'     \item \code{country}: Most frequent residence/country for this assignee
#'     \item \code{country_pct}: Percentage of records with this country (0-100)
#'     \item \code{address}: Most frequent address for this assignee
#'     \item \code{address_pct}: Percentage of records with this address (0-100)
#'     \item \code{total_records}: Total number of patent records for this assignee
#'   }
#'
#' @details
#' The function handles missing values (NA) by excluding them from frequency
#' calculations. If all values for a given assignee are NA, the function returns
#' NA for that field.
#'
#' When there are ties (multiple countries or addresses with the same maximum
#' frequency), the function returns the first one alphabetically.
#'
#' The percentage values help identify assignees with consistent location
#' information (high percentages) versus those with high variability (low percentages).
#'
#' @import dplyr
#' @importFrom rlang sym
#'
#' @examples
#' # Create sample patent data with standard column names
#' patent_data <- data.frame(
#'   lens_id = c("L1", "L2", "L3", "L4", "L5"),
#'   sequence = c(1, 1, 1, 2, 2),
#'   name = c("Acme Corp", "Acme Corp", "Acme Corp", "Tech Inc", "Tech Inc"),
#'   residence = c("USA", "USA", "Canada", "Germany", "Germany"),
#'   address = c("New York", "New York", "Toronto", "Berlin", "Munich"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Using default column names
#' result <- extract_assignee_country_unique(patent_data)
#'
#' # Create data with different column names
#' patent_data2 <- data.frame(
#'   applicant = c("Acme Corp", "Acme Corp", "Tech Inc"),
#'   country = c("USA", "USA", "Germany"),
#'   location = c("New York", "New York", "Berlin"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Using custom column names
#' result2 <- extract_assignee_country_unique(patent_data2,
#'                                  name_col = "applicant",
#'                                  residence_col = "country",
#'                                  address_col = "location")
#'
#' @export
extract_assignee_country_unique <- function(df,
                                 name_col = "name",
                                 residence_col = "residence",
                                 address_col = "address") {

  # Check required columns exist
  required_cols <- c(name_col, residence_col, address_col)
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Convert column names to symbols for tidy evaluation
  name_sym <- rlang::sym(name_col)
  residence_sym <- rlang::sym(residence_col)
  address_sym <- rlang::sym(address_col)

  df %>%
    dplyr::group_by(!!name_sym) %>%
    dplyr::summarise(
      country = {
        valid_vals <- !!residence_sym
        valid_vals <- valid_vals[!is.na(valid_vals)]
        if(length(valid_vals) == 0) {
          NA_character_
        } else {
          names(which.max(table(valid_vals)))
        }
      },
      country_pct = {
        valid_vals <- !!residence_sym
        valid_vals <- valid_vals[!is.na(valid_vals)]
        if(length(valid_vals) == 0) {
          NA_real_
        } else {
          max(table(valid_vals)) / length(valid_vals) * 100
        }
      },
      address = {
        valid_vals <- !!address_sym
        valid_vals <- valid_vals[!is.na(valid_vals)]
        if(length(valid_vals) == 0) {
          NA_character_
        } else {
          names(which.max(table(valid_vals)))
        }
      },
      address_pct = {
        valid_vals <- !!address_sym
        valid_vals <- valid_vals[!is.na(valid_vals)]
        if(length(valid_vals) == 0) {
          NA_real_
        } else {
          max(table(valid_vals)) / length(valid_vals) * 100
        }
      },
      total_records = dplyr::n(),
      .groups = 'drop'
    )
}
