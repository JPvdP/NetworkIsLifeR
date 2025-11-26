#' setup_bertopic_env
#'
#' This function checks for a working Python installation and installs one
#' if none is available. It then creates a dedicated Python virtual
#' environment and installs the Python packages required for using
#' `bertopicR`. If the environment already exists, the function exits
#' safely without making changes. This ensures that all Python
#' dependencies needed by `bertopicR` are available and isolated in a
#' controlled environment.
#'
#' @param envname The name of the python virtual environment.
#' @return Creates a virtual environment and installs the required python-base.
#' @examples
#' setup_bertopic_env("bertopic_r_env")
#' @export
setup_bertopic_env <- function(envname = "bertopic_r_env",
                               python_version = "3.11") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required but not installed.")
  }

  is_windows <- identical(.Platform$OS.type, "windows")

  # ---- 1. Ensure some Python toolchain exists ----
  # This uses reticulate's helpers so you don't care where Python lives.
  if (!reticulate::py_available(initialize = FALSE)) {
    if (is_windows) {
      message("No Python detected. Installing Miniconda (recommended on Windows)...")
      reticulate::install_miniconda()
    } else {
      message("No Python detected. Installing a standalone Python distribution...")
      reticulate::install_python(version = python_version)
    }
  }

  # ---- 2. If env already exists, bail out early ----
  if (is_windows) {
    existing_envs <- tryCatch(
      reticulate::conda_list()$name,
      error = function(e) character()
    )
  } else {
    existing_envs <- reticulate::virtualenv_list()
  }

  if (envname %in% existing_envs) {
    message("Environment '", envname, "' already exists. Nothing done.")
    return(invisible(FALSE))
  }

  # NOTE: from here on, we assume this is called in a fresh R session.
  # If Python was already initialized earlier in the session,
  # use_*() won't be able to change it.
  # So: call this function BEFORE using Python in the session.

  # ---- 3. Create the environment (OS-specific) ----
  if (is_windows) {
    # ----- Windows: use conda envs -----
    message("Creating conda environment '", envname, "'...")
    reticulate::conda_create(
      envname = envname,
      packages = paste0("python=", python_version)
    )

    reticulate::use_condaenv(envname, required = TRUE)

    message("Installing Python packages into '", envname, "' (conda)...")
    reticulate::py_install(
      packages = c("sentence-transformers", "numpy"),
      envname  = envname,
      method   = "conda"
    )

  } else {
    # ----- macOS / Linux: use virtualenv -----
    message("Creating virtualenv '", envname, "'...")
    # Let reticulate find a suitable Python; you can also force a version:
    reticulate::virtualenv_create(
      envname = envname,
      python  = python_version,
      packages = FALSE  # we'll install packages explicitly below
    )

    reticulate::use_virtualenv(envname, required = TRUE)

    message("Installing Python packages into '", envname, "' (virtualenv)...")
    reticulate::py_install(
      packages = c("sentence-transformers", "numpy"),
      envname  = envname,
      method   = "virtualenv"
    )
  }

  message("Environment setup complete: '", envname, "'.")
  invisible(TRUE)
}


