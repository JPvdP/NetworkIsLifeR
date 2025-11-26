#' RCA_fractional
#'
#' This function computes the Revealed Comparative Advantage (RCA) of regions
#' based on IPC (International Patent Classification) technology classes using
#' fractional counting. Fractional counting is required because one patent can be
#' assigned to multiple regions (e.g., multiple applicants located in different
#' regions). Each patent-region-IPC combination receives a weight equal to
#' `1 / (number of regions associated with that patent)`.
#'
#' @param df A dataframe containing the patent data in long format.
#'           Each row must represent one combination of:
#'           * a patent ID
#'           * one IPC class
#'           * one region
#'
#' @param id_col A string giving the column name containing the patent identifier
#'               (e.g., `"Lens_ID"` or `"patent_id"`).
#'
#' @param ipc_col A string giving the column name containing IPC classifications
#'                (e.g., `"IPC"` or `"ipc_code"`).
#'                IPCs can be sections, classes, subclasses, or full codes.
#'
#' @param region_col A string giving the column name containing the geographic
#'                   region of the applicant (e.g., `"Region"` or `"province"`).
#'                   If a patent is associated with multiple regions, the input
#'                   dataframe should include multiple rows (one per region).
#'
#' @return A dataframe containing fractional IPC counts by region and their RCA
#'         values. Columns include:
#'
#' \describe{
#'   \item{Region}{The region, as provided in `region_col`.}
#'
#'   \item{IPC}{The IPC class, as provided in `ipc_col`.}
#'
#'   \item{X_rc}{The fractional count of IPC occurrences in this region.
#'               If a patent has multiple regions, each region receives
#'               a fractional weight of `1 / number_of_regions`.}
#'
#'   \item{X_r}{The total fractional IPC count for the region
#'              (sum of all X_rc across all IPC classes in that region).}
#'
#'   \item{X_c}{The total fractional count for that IPC class across all regions.}
#'
#'   \item{share_region}{The region's specialization share in this IPC:
#'                       \eqn{X_rc / X_r}.}
#'
#'   \item{share_global}{The global share of this IPC across all regions:
#'                       \eqn{X_c / sum(X_rc) }.}
#'
#'   \item{RCA}{The Revealed Comparative Advantage value:
#'              \eqn{(X_rc / X_r) / (X_c / sum(X_rc))}.
#'              Values above 1 indicate regional specialization in the IPC.}
#' }
#'
#' @details
#' The function uses fractional counting to avoid over-representing patents that
#' have multiple applicants across several regions. It assumes that IPC codes and
#' region links are already assigned at the patent level. Duplicate rows (e.g.,
#' repeated IPC-region combinations for the same patent) are removed automatically.
#'
#' @examples
#' df <- data.frame(
#'   Lens_ID = c("A","A","A","B","B","C"),
#'   IPC     = c("H04W","G06F","H04W","A61K","A61K","G06F"),
#'   Region  = c("North","North","South","South","North","North")
#' )
#'
#' compute_rca_fractional(df,
#'   id_col = "Lens_ID",
#'   ipc_col = "IPC",
#'   region_col = "Region"
#' )
#'
