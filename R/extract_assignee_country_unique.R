#' Assign Most Frequent Country and Address to Each Assignee
#'
#' This function takes a patent database with multiple rows per assignee and
#' assigns the most frequent country (residence) and address to each unique
#' assignee name. This is useful for consolidating assignee information when
#' the same entity appears with different locations across multiple patents.
#'
#' @param df A data frame containing patent assignee information with at least
#'   the following columns:
#'   \itemize{
#'     \item \code{name}: Character vector with assignee names
#'     \item \code{residence}: Character vector with country information
#'     \item \code{address}: Character vector with address information
#'   }
#'
#' @return A data frame with one row per unique assignee name containing:
#'   \itemize{
#'     \item \code{name}: Unique assignee name
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
#'
#' @examples
#' # Create sample patent data
#' patent_data <- data.frame(
#'   lens_id = c("L1", "L2", "L3", "L4", "L5"),
#'   sequence = c(1, 1, 1, 2, 2),
#'   name = c("Acme Corp", "Acme Corp", "Acme Corp", "Tech Inc", "Tech Inc"),
#'   residence = c("USA", "USA", "Canada", "Germany", "Germany"),
#'   address = c("New York", "New York", "Toronto", "Berlin", "Munich"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Assign most frequent country and address
#' result <- extract_assignee_country_unique(patent_data)
#' print(result)
#' #   name       country country_pct address  address_pct total_records
#' #   Acme Corp  USA     66.7        New York 66.7        3
#' #   Tech Inc   Germany 100.0       Berlin   50.0        2
#'
#' @export
extract_assignee_country_unique <- function(df) {
  # Check required columns exist
  required_cols <- c("name", "residence", "address")
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  df %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(
      country = {
        valid_vals <- residence[!is.na(residence)]
        if(length(valid_vals) == 0) {
          NA_character_
        } else {
          names(which.max(table(valid_vals)))
        }
      },
      country_pct = {
        valid_vals <- residence[!is.na(residence)]
        if(length(valid_vals) == 0) {
          NA_real_
        } else {
          max(table(valid_vals)) / length(valid_vals) * 100
        }
      },
      address = {
        valid_vals <- address[!is.na(address)]
        if(length(valid_vals) == 0) {
          NA_character_
        } else {
          names(which.max(table(valid_vals)))
        }
      },
      address_pct = {
        valid_vals <- address[!is.na(address)]
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
