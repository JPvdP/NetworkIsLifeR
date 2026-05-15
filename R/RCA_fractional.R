#' Compute Revealed Comparative Advantage (RCA) for Patent Data
#'
#' Computes Revealed Comparative Advantage (RCA) indices for patent
#' classifications across regions, using fractional counting to handle
#' patents assigned to multiple regions. Supports static (pooled) and
#' dynamic (per-year) computation, optional filtering of sparse cells,
#' and optional trailing moving-average smoothing of the underlying
#' counts.
#'
#' The RCA index follows the Balassa (1965) formulation adapted to
#' patents: for a region \eqn{r} and IPC class \eqn{c},
#' \deqn{RCA_{rc} = \frac{X_{rc} / X_{r}}{X_{c} / X_{total}}}
#' where \eqn{X_{rc}} is the fractional count of patents in region
#' \eqn{r} classified in IPC \eqn{c}, \eqn{X_{r}} is the regional
#' total, \eqn{X_{c}} is the IPC total, and \eqn{X_{total}} is the
#' overall total. Values greater than 1 indicate that the region is
#' relatively specialised in the IPC class.
#'
#' Fractional counting is applied at the region level: each patent
#' contributes a weight of \eqn{1 / n_{regions}} to each region it
#' is assigned to, so the total weight per patent always sums to 1.
#' In dynamic mode, fractional weights are computed within each
#' patent-year combination.
#'
#' @param df A data frame containing one row per
#'   patent-IPC-region (-year) classification. Rows may repeat across
#'   IPCs or regions; the function deduplicates internally.
#' @param id_col Character. Name of the column identifying patents
#'   (e.g. `"Lens_ID"`).
#' @param ipc_col Character. Name of the column with IPC (or other
#'   technology) classifications.
#' @param region_col Character. Name of the column identifying
#'   regions, countries, or other geographic units.
#' @param year_col Character or `NULL`. Name of the year column. If
#'   `NULL` (default), RCA is computed once over the full pooled
#'   sample. If supplied, RCA is computed separately for each year.
#' @param min_patents Integer. Minimum number of distinct patents
#'   required for a region-IPC (or region-IPC-year, in dynamic mode)
#'   cell to be retained. Cells below the threshold are dropped
#'   before fractional weights are computed. Defaults to `0` (no
#'   filtering).
#' @param ma_window Integer or `NULL`. Window size, in years, for a
#'   trailing moving average applied to the underlying counts
#'   (`X_rc`, `X_r`, `X_c`, `X_total`) before recomputing RCA.
#'   Requires `year_col` to be supplied. If `NULL` (default), no
#'   smoothing is applied. A value of `3` reproduces the original
#'   three-year trailing moving average behaviour.
#'
#' @return A tibble with one row per region-IPC (or region-IPC-year)
#'   combination. Columns include:
#'   \describe{
#'     \item{`region_col`, `ipc_col`, (`year_col`)}{Grouping variables.}
#'     \item{`X_rc`}{Fractional patent count for the region-IPC cell.}
#'     \item{`X_r`}{Fractional total for the region.}
#'     \item{`X_c`}{Fractional total for the IPC.}
#'     \item{`X_total`}{Overall fractional total.}
#'     \item{`share_region`}{`X_rc / X_r`.}
#'     \item{`share_global`}{`X_c / X_total`.}
#'     \item{`RCA`}{Revealed Comparative Advantage index. Renamed to
#'       `RCA_raw` when `ma_window` is supplied.}
#'   }
#'   When `ma_window` is supplied, additional columns are returned:
#'   `X_rc_ma`, `X_r_ma`, `X_c_ma`, `X_total_ma`, `share_region_ma`,
#'   `share_global_ma`, and `RCA_ma` (the smoothed index).
#'
#' @details
#' Setting `min_patents > 0` is useful for reducing noise in dynamic
#' RCAs, where small year-cells can produce extreme index values.
#' The filter is applied on raw distinct patent counts (not
#' fractional weights), so the threshold has an intuitive
#' interpretation.
#'
#' When `ma_window` is supplied, the moving average is applied to
#' the underlying count series rather than to the RCA index itself.
#' This is generally preferable because averaging ratios directly
#' can be biased; smoothing the counts and recomputing the ratio
#' avoids this.
#'
#' @section Required packages:
#' `dplyr` and `rlang` are required. `slider` is required only when
#' `ma_window` is supplied.
#'
#' @references
#' Balassa, B. (1965). Trade Liberalisation and "Revealed"
#' Comparative Advantage. *The Manchester School*, 33(2), 99-123.
#'
#' @examples
#' \dontrun{
#' # Static RCA across the full sample
#' RCA_fractional(patents, "Lens_ID", "IPC", "Region")
#'
#' # Dynamic RCA, one value per region-IPC-year
#' RCA_fractional(patents, "Lens_ID", "IPC", "Region",
#'             year_col = "Year")
#'
#' # Dynamic RCA, dropping cells with fewer than 5 patents
#' RCA_fractional(patents, "Lens_ID", "IPC", "Region",
#'             year_col = "Year", min_patents = 5)
#'
#' # Dynamic RCA with a 3-year trailing moving average
#' RCA_fractional(patents, "Lens_ID", "IPC", "Region",
#'             year_col = "Year", ma_window = 3)
#' }
#'
#' @importFrom dplyr distinct group_by summarise mutate left_join
#'   inner_join arrange ungroup count rename across all_of
#' @importFrom rlang sym !!
#' @export
RCA_fractional <- function(df,
                        id_col,
                        ipc_col,
                        region_col,
                        year_col    = NULL,   # NULL = static RCA; otherwise dynamic per year
                        min_patents = 0,      # 0 = no filter; >0 = drop region-IPC(-year) cells below threshold
                        ma_window   = NULL) { # NULL = no smoothing; integer = trailing MA window (years)


  dynamic <- !is.null(year_col)
  if (!is.null(ma_window) && !dynamic) {
    stop("ma_window requires year_col (moving average needs a time dimension).")
  }

  # Symbols
  id_sym     <- sym(id_col)
  ipc_sym    <- sym(ipc_col)
  region_sym <- sym(region_col)
  year_sym   <- if (dynamic) sym(year_col) else NULL

  # Grouping keys reused below
  rc_keys  <- if (dynamic) c(region_col, ipc_col, year_col) else c(region_col, ipc_col)
  r_keys   <- if (dynamic) c(region_col, year_col)          else region_col
  c_keys   <- if (dynamic) c(ipc_col, year_col)             else ipc_col
  pid_keys <- if (dynamic) c(id_col, year_col)              else id_col

  #------------------------------------------------------------
  # 1. Deduplicate (patent × IPC × region [× year])
  #------------------------------------------------------------
  df_unique <- if (dynamic) {
    df %>% distinct(!!id_sym, !!ipc_sym, !!region_sym, !!year_sym)
  } else {
    df %>% distinct(!!id_sym, !!ipc_sym, !!region_sym)
  }

  #------------------------------------------------------------
  # 2. Optional: filter region-IPC(-year) cells with too few patents
  #------------------------------------------------------------
  if (min_patents > 0) {
    patent_counts <- df_unique %>%
      group_by(across(all_of(rc_keys))) %>%
      summarise(n_patents = n_distinct(!!id_sym), .groups = "drop") %>%
      filter(n_patents >= min_patents)

    df_unique <- df_unique %>%
      inner_join(patent_counts, by = rc_keys)
  }

  #------------------------------------------------------------
  # 3. Fractional region weights (per patent[-year])
  #------------------------------------------------------------
  region_count <- df_unique %>%
    distinct(across(all_of(c(pid_keys, region_col)))) %>%
    count(across(all_of(pid_keys)), name = "n_regions")

  df_frac <- df_unique %>%
    left_join(region_count, by = pid_keys) %>%
    mutate(weight = 1 / n_regions)

  #------------------------------------------------------------
  # 4. Fractional IPC counts per region(-year) + totals
  #------------------------------------------------------------
  counts <- df_frac %>%
    group_by(across(all_of(rc_keys))) %>%
    summarise(X_rc = sum(weight), .groups = "drop")

  totals_region <- counts %>%
    group_by(across(all_of(r_keys))) %>%
    summarise(X_r = sum(X_rc), .groups = "drop")

  totals_ipc <- counts %>%
    group_by(across(all_of(c_keys))) %>%
    summarise(X_c = sum(X_rc), .groups = "drop")

  if (dynamic) {
    totals_year <- counts %>%
      group_by(!!year_sym) %>%
      summarise(X_total = sum(X_rc), .groups = "drop")
  }

  #------------------------------------------------------------
  # 5. Assemble RCA
  #------------------------------------------------------------
  rca <- counts %>%
    left_join(totals_region, by = r_keys) %>%
    left_join(totals_ipc,    by = c_keys)

  if (dynamic) {
    rca <- rca %>% left_join(totals_year, by = year_col)
  } else {
    rca <- rca %>% mutate(X_total = sum(counts$X_rc))
  }

  rca <- rca %>%
    mutate(
      share_region = X_rc / X_r,
      share_global = X_c  / X_total,
      RCA          = share_region / share_global
    )

  if (!dynamic) {
    rca <- rca %>% arrange(!!region_sym, desc(RCA))
  }

  #------------------------------------------------------------
  # 6. Optional: trailing moving average on smoothed counts
  #------------------------------------------------------------
  if (!is.null(ma_window) && ma_window > 1) {
    if (!requireNamespace("slider", quietly = TRUE)) {
      stop("Package 'slider' is required for ma_window. install.packages('slider').")
    }
    library(slider)

    rca <- rca %>%
      rename(RCA_raw = RCA) %>%
      arrange(!!region_sym, !!ipc_sym, !!year_sym) %>%
      group_by(!!region_sym, !!ipc_sym) %>%
      mutate(
        X_rc_ma    = slide_dbl(X_rc,    mean, .before = ma_window - 1, .complete = TRUE),
        X_r_ma     = slide_dbl(X_r,     mean, .before = ma_window - 1, .complete = TRUE),
        X_c_ma     = slide_dbl(X_c,     mean, .before = ma_window - 1, .complete = TRUE),
        X_total_ma = slide_dbl(X_total, mean, .before = ma_window - 1, .complete = TRUE),
        share_region_ma = X_rc_ma / X_r_ma,
        share_global_ma = X_c_ma  / X_total_ma,
        RCA_ma          = share_region_ma / share_global_ma
      ) %>%
      ungroup()
  }

  return(rca)
}

