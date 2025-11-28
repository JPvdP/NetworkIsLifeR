#' accept_anaconda_tos
#'
#' Reorder factor levels within each topic panel.
#' Based on tidytext's reorder_within().
#'
#' @keywords internal
accept_anaconda_tos <- function(conda_bin = NULL) {
  if (is.null(conda_bin)) {
    conda_bin <- reticulate::conda_binary()
  }
  if (is.null(conda_bin) || !nzchar(conda_bin)) {
    stop("Could not find a conda binary. Is Miniconda installed?")
  }

  channels <- c(
    "https://repo.anaconda.com/pkgs/main",
    "https://repo.anaconda.com/pkgs/r",
    "https://repo.anaconda.com/pkgs/msys2"
  )

  message("Accepting Anaconda Terms of Service for default channels via `conda tos accept`...")

  for (ch in channels) {
    args <- c("tos", "accept", "--override-channels", "--channel", ch)
    status <- system2(conda_bin, args = args)
    if (!identical(status, 0L)) {
      stop("Failed to accept ToS for channel: ", ch,
           ". Please try manually in a terminal.")
    }
  }

  invisible(TRUE)
}
