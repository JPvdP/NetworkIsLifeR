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
setup_bertopic_env <- function(envname = "bertopic_r_env") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required but not installed.")
  }

  # ---- 1. Ensure Python exists ----
  if (!reticulate::py_available(initialize = FALSE)) {
    message("No Python detected. Installing a standalone Python distribution...")
    reticulate::install_python()
  }

  # ---- 2. Ensure 'virtualenv' is installed ----
  py_bin <- reticulate::py_discover_config()$python

  has_virtualenv <- tryCatch(
    system2(py_bin, args = c("-m", "virtualenv", "--version"),
            stdout = TRUE, stderr = TRUE),
    error = function(e) FALSE
  )

  if (identical(has_virtualenv, FALSE)) {
    message("Installing 'virtualenv' into your Python setup...")
    reticulate::py_install("virtualenv")
  }

  # ---- 3. Stop if environment already exists ----
  existing_envs <- reticulate::virtualenv_list()
  if (envname %in% existing_envs) {
    message("Virtual environment '", envname, "' already exists. Nothing done.")
    return(invisible(FALSE))
  }

  # ---- 4. Prevent initialization if Python is already active ----
  if (reticulate::py_available()) {
    stop("Python is already initialized. Restart R before running setup_bertopic_env().")
  }

  # ---- 5. Create the environment ----
  message("Creating virtualenv '", envname, "'...")
  reticulate::virtualenv_create(envname)

  reticulate::use_virtualenv(envname, required = TRUE)

  # ---- 6. Install required packages ----
  message("Installing Python packages into '", envname, "'...")
  reticulate::py_install(
    c("sentence-transformers", "numpy"),
    envname = envname
  )

  message("Environment setup complete!")
  invisible(TRUE)
}

