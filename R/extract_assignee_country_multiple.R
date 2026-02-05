#' Summarize Assignee Information by Country
#'
#' This function takes a patent database with multiple rows per assignee and
#' creates one row for each unique assignee-country pair. For each pair, it
#' identifies the most frequent address and calculates the percentage of the
#' assignee's total patents associated with each country.
#'
#' @param df A data frame containing patent assignee information.
#' @param name_col Character string specifying the column name containing
#'   assignee names. Default is "name".
#' @param residence_col Character string specifying the column name containing
#'   country/residence information. Default is "residence".
#' @param address_col Character string specifying the column name containing
#'   address information. Default is "address".
#'
#' @return A data frame with one row per unique assignee-country pair containing:
#'   \itemize{
#'     \item Column with assignee names (using the name specified in \code{name_col})
#'     \item \code{residence}: Country for this assignee-country pair
#'     \item \code{country_records}: Number of patent records for this name-country combination
#'     \item \code{country_pct}: Percentage of this assignee's total patents from this country (0-100)
#'     \item \code{total_records}: Total number of patent records across all countries for this assignee
#'     \item \code{address}: Most frequent address for this name-country pair
#'     \item \code{address_pct}: Percentage of records with this address within this name-country pair (0-100)
#'   }
#'   The results are sorted by assignee name and then by descending country_records.
#'
#' @details
#' The function filters out rows where either the name column or residence column is NA.
#' For the address field, NA values are excluded from frequency calculations.
#' If all addresses for a name-country pair are NA, the function returns NA for
#' the address fields.
#'
#' When there are ties in address frequency, the function returns the first one
#' alphabetically.
#'
#' This function is particularly useful for:
#' \itemize{
#'   \item Identifying multinational assignees and their geographic distribution
#'   \item Understanding the relative importance of different countries for each assignee
#'   \item Detecting potential data quality issues (e.g., assignees split across many countries)
#' }
#'
#' @import dplyr
#' @importFrom rlang sym
#'
#' @examples
#' # Create sample patent data with standard column names
#' patent_data <- data.frame(
#'   lens_id = c("L1", "L2", "L3", "L4", "L5", "L6"),
#'   sequence = c(1, 1, 1, 2, 2, 3),
#'   name = c("Global Corp", "Global Corp", "Global Corp",
#'            "Global Corp", "Global Corp", "Local Inc"),
#'   residence = c("USA", "USA", "USA", "Germany", "Germany", "Canada"),
#'   address = c("New York", "New York", "Boston", "Berlin", "Berlin", "Toronto"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Using default column names
#' result <- extract_assignee_country_multiple(patent_data)
#'
#' # Create data with different column names
#' patent_data2 <- data.frame(
#'   applicant = c("Global Corp", "Global Corp", "Local Inc"),
#'   country = c("USA", "Germany", "Canada"),
#'   location = c("New York", "Berlin", "Toronto"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Using custom column names
#' result2 <- extract_assignee_country_multiple(patent_data2,
#'                                      name_col = "applicant",
#'                                      residence_col = "country",
#'                                      address_col = "location")
#'
#' @seealso \code{\link{extract_assignee_country_unique}} for assigning a single country
#'   and address to each assignee.
#'
#' @export
extract_assignee_country_multiple <- function(df,
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
    dplyr::filter(!is.na(!!name_sym) & !is.na(!!residence_sym)) %>%
    dplyr::group_by(!!name_sym, !!residence_sym) %>%
    dplyr::summarise(
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
      country_records = dplyr::n(),
      .groups = 'drop'
    ) %>%
    dplyr::group_by(!!name_sym) %>%
    dplyr::mutate(
      total_records = sum(country_records),
      country_pct = country_records / total_records * 100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(!!name_sym, residence = !!residence_sym, country_records,
                  country_pct, total_records, address, address_pct) %>%
    dplyr::arrange(!!name_sym, dplyr::desc(country_records))
}
