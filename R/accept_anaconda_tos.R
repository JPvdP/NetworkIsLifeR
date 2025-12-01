#' accept_anaconda_tos
#' @keywords internal
accept_anaconda_tos <- function() {
  # Tell the conda ToS plugin to auto-accept terms for this R session.
  # Only called AFTER the user has confirmed in an interactive prompt.
  Sys.setenv(CONDA_PLUGINS_AUTO_ACCEPT_TOS = "yes")
  invisible(TRUE)
}
