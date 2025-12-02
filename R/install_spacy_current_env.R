#' Install or repair spaCy in the currently active Python environment
#'
#' This installs spaCy (via pip) and optionally a language model into the
#' Python environment that reticulate is currently attached to.
#'
#' Typical workflow:
#'   - First attach the environment (use_virtualenv() / use_condaenv()
#'     / start_bertopicr()).
#'   - Then run install_spacy_current_env() if spaCy or the model is missing.
#'
#' @param model Name of the spaCy model to download, or NULL to skip
#'   model installation (default "en_core_web_sm").
#' @export
install_spacy_current_env <- function(model = "en_core_web_sm") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required but not installed.")
  }

  # Make sure Python is attached
  if (!reticulate::py_available(initialize = TRUE)) {
    stop(
      "Python is not available via reticulate.\n",
      "Attach an environment first (start_bertopicr(), use_virtualenv(), or use_condaenv())."
    )
  }

  cfg <- reticulate::py_config()
  python_bin <- normalizePath(cfg$python, winslash = "/")
  message("Using Python:\n  ", python_bin)

  message("Installing spaCy in this environment via pip ...")
  # pip install spacy
  system2(python_bin,
          c("-m", "pip", "install", "spacy"),
          stdout = TRUE, stderr = TRUE)

  if (!is.null(model)) {
    message("Downloading spaCy model '", model, "' in this environment ...")
    # python -m spacy download en_core_web_sm
    system2(python_bin,
            c("-m", "spacy", "download", model),
            stdout = TRUE, stderr = TRUE)
  }

  message("spaCy (and model, if requested) installed successfully.")
  invisible(TRUE)
}
