#' Compute fractional counts by region (or any grouping column) within an ID.
#'
#' For each ID (e.g., publication), this computes the share of "units" that fall in each
#' region/group. The default interpretation is:
#'
#' \deqn{fraction_{(id, region)} = n_{units \in region} / n_{units \in id}}
#'
#' Example: if an article (EID) has 4 affiliations from region A and 4 from region B,
#' then each region gets 4/8 = 0.5 for that EID.
#'
#' @param df A data.frame / tibble.
#' @param id_col Column name (string) that identifies the item to fractionally allocate
#'   (e.g., publication id like "EID").
#' @param region_col Column name (string) that defines the regional/group level
#'   (e.g., "Provincie_DEP").
#' @param unit_col Optional column name (string) defining the unit to count within an ID
#'   (e.g., affiliation id/name like "Clean"). If NULL, each row is treated as 1 unit.
#' @param by_cols Optional character vector of additional columns to compute fractions within
#'   (e.g., c("Year") to compute per Year x EID).
#' @param distinct_unit Logical. If TRUE and unit_col is provided, counts *unique* units
#'   per (by_cols, id, region). (Recommended when unit_col is an affiliation name.)
#' @param drop_na Logical. If TRUE, drops rows with NA in any required columns.
#' @param frac_name Name of the output fractional count column.
#' @param n_total_name Name of the output total-units-per-id column.
#' @param n_region_name Name of the output units-per-(id,region) column.
#' @param validate_sum Logical. If TRUE, warns when fractional shares don't sum ~1 per ID
#'   (within by_cols, if provided).
#'
#' @return A tibble with columns: by_cols (if any), id_col, region_col,
#'   n_total_name, n_region_name, frac_name.
#'
#' @examples
#' # Fractional affiliations by province within each publication:
#' # out <- compute_fractional_counts(
#' #   df = Scopus_affiliations_NL_NDS,
#' #   id_col = "EID",
#' #   region_col = "Provincie_DEP",
#' #   unit_col = "Clean"
#' # )
#'
#' # If you want fractions per Year x EID:
#' # out_y <- compute_fractional_counts(
#' #   df = Scopus_affiliations_NL_NDS,
#' #   id_col = "EID",
#' #   region_col = "Provincie_DEP",
#' #   unit_col = "Clean",
#' #   by_cols = c("Year")
#' # )
#'
#' @export
compute_fractional_counts <- function(df,
                                      id_col,
                                      region_col,
                                      unit_col = NULL,
                                      by_cols = NULL,
                                      distinct_unit = TRUE,
                                      drop_na = TRUE,
                                      frac_name = "fractional_count",
                                      n_total_name = "n_unit_total",
                                      n_region_name = "n_unit_region",
                                      validate_sum = TRUE) {
  # --- checks ---
  if (!is.data.frame(df)) stop("`df` must be a data.frame or tibble.")
  if (!is.character(id_col) || length(id_col) != 1) stop("`id_col` must be a single string.")
  if (!is.character(region_col) || length(region_col) != 1) stop("`region_col` must be a single string.")
  if (!is.null(unit_col) && (!is.character(unit_col) || length(unit_col) != 1)) {
    stop("`unit_col` must be NULL or a single string.")
  }
  if (!is.null(by_cols) && (!is.character(by_cols) || length(by_cols) < 1)) {
    stop("`by_cols` must be NULL or a character vector of column names.")
  }

  required <- c(by_cols, id_col, region_col, unit_col)
  required <- required[!is.null(required)]
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing column(s) in `df`: ", paste(missing_cols, collapse = ", "))
  }

  # --- select relevant columns only (stable + fast) ---
  x <- df[, required, drop = FALSE]

  # --- drop NA if requested ---
  if (isTRUE(drop_na)) {
    keep <- stats::complete.cases(x)
    x <- x[keep, , drop = FALSE]
  }

  # --- optionally de-duplicate units within (by,id,region) ---
  if (!is.null(unit_col) && isTRUE(distinct_unit)) {
    # unique per by_cols + id + region + unit
    dedup_cols <- c(by_cols, id_col, region_col, unit_col)
    x <- dplyr::distinct(x, dplyr::across(dplyr::all_of(dedup_cols)))
  }

  # --- counts per (by,id,region) ---
  group_region <- c(by_cols, id_col, region_col)
  region_counts <- x %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_region))) %>%
    dplyr::summarise(
      !!n_region_name := dplyr::n(),
      .groups = "drop"
    )

  # --- totals per (by,id) ---
  group_id <- c(by_cols, id_col)
  total_counts <- x %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_id))) %>%
    dplyr::summarise(
      !!n_total_name := dplyr::n(),
      .groups = "drop"
    )

  # --- join + fractional ---
  out <- dplyr::left_join(region_counts, total_counts, by = group_id) %>%
    dplyr::mutate(
      !!frac_name := .data[[n_region_name]] / .data[[n_total_name]]
    )

  # --- optional validation: sum of fractions per (by,id) should be 1 ---
  if (isTRUE(validate_sum)) {
    check <- out %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_id))) %>%
      dplyr::summarise(sum_frac = sum(.data[[frac_name]]), .groups = "drop")

    bad <- check %>%
      dplyr::filter(!is.finite(sum_frac) | abs(sum_frac - 1) > 1e-8)

    if (nrow(bad) > 0) {
      warning(
        "Fractional counts do not sum to ~1 for ",
        nrow(bad), " group(s) (within by_cols + id). ",
        "This can happen if rows were dropped (NA) or if region_col is missing for some units."
      )
    }
  }

  out
}
