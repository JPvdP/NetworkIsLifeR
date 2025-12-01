#' setup_bertopic_env
#'
#' This function checks for a working Python installation and installs one
#' if none is available. It then creates a dedicated Python virtual
#' environment and installs the Python packages required for using
#' `bertopicR`. If the environment already exists, the function exits
#' safely without making changes. This ensures that all Python
#' dependencies needed by `bertopicR` are available and isolated in a
#' controlled environment.
#' @param envname Name of the environment.
#' @param python_version Python version to install/use (default "3.11").
#' @param use_conda_on_windows Logical; if TRUE on Windows use conda+Miniconda,
#'   otherwise fall back to virtualenv.
#' @export
setup_bertopic_env <- function(envname = "bertopic_r_env",
                               python_version = "3.11",
                               use_conda_on_windows = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required but not installed.")
  }

  is_windows <- identical(.Platform$OS.type, "windows")

  # -------------------------------------------------------------------
  # WINDOWS + conda branch
  # -------------------------------------------------------------------
  if (is_windows && use_conda_on_windows) {

    # 1) Check if we already have a conda binary; if not, install Miniconda
    conda_bin <- tryCatch(
      reticulate::conda_binary(),
      error = function(e) ""
    )

    if (!nzchar(conda_bin)) {
      message("No conda found. Installing Miniconda for reticulate...")
      reticulate::install_miniconda()

      # Re-resolve the conda binary after install
      conda_bin <- tryCatch(
        reticulate::conda_binary(),
        error = function(e) ""
      )
      if (!nzchar(conda_bin)) {
        stop("Could not find Miniconda / conda binary even after install.")
      }
    }

    # 2) If env already exists, bail out
    existing_envs <- tryCatch(
      reticulate::conda_list()$name,
      error = function(e) character()
    )
    if (envname %in% existing_envs) {
      message("Conda environment '", envname, "' already exists. Nothing done.")
      return(invisible(FALSE))
    }

    # 3) Interactive ToS prompt for Anaconda channels
    if (interactive()) {
      cat(
        "To create a conda environment, conda may use the Anaconda default channels,\n",
        "which are protected by Terms of Service.\n\n",
        "By confirming below, this function will run the equivalent of:\n",
        "  conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/main\n",
        "  conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/r\n",
        "  conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/msys2\n\n",
        "You should read the Terms of Service here:\n",
        "  https://www.anaconda.com/legal/terms-of-service\n\n",
        sep = ""
      )

      ans <- readline("Do you confirm that you have read and agree to these Terms of Service? [yes/no]: ")
      ans <- tolower(trimws(ans))

      if (ans %in% c("yes", "y")) {
        # <- this is your internal helper
        accept_anaconda_tos(conda_bin)
      } else {
        stop("You did not accept the Terms of Service. Cannot proceed with conda-based setup.")
      }

    } else {
      stop(
        "Conda may require accepting Anaconda's Terms of Service, but this R session is non-interactive.\n",
        "Please run setup_bertopic_env() interactively once, or accept the ToS manually\n",
        "using the `conda tos accept` commands suggested by conda."
      )
    }

    # 4) Create the conda env
    message("Creating conda environment '", envname, "'...")
    reticulate::conda_create(
      envname  = envname,
      packages = paste0("python=", python_version)
    )

    reticulate::use_condaenv(envname, required = TRUE)

    message("Installing Python packages into '", envname, "' (conda)...")
    reticulate::py_install(
      packages = c("sentence-transformers", "numpy"),
      envname  = envname,
      method   = "conda"
    )

    message("Environment setup complete (conda, Windows).")
    return(invisible(TRUE))
  }

  # -------------------------------------------------------------------
  # macOS / Linux / (Windows with use_conda_on_windows = FALSE)
  # virtualenv-based branch
  # -------------------------------------------------------------------

  # 1) Ensure some Python exists (standalone or system)
  if (!reticulate::py_available(initialize = FALSE)) {
    message("No Python detected by reticulate. Installing Python ", python_version, " ...")
    reticulate::install_python(python_version)
  }

  # 2) If virtualenv already exists, bail out
  existing_envs <- reticulate::virtualenv_list()
  if (envname %in% existing_envs) {
    message("Virtual environment '", envname, "' already exists. Nothing done.")
    return(invisible(FALSE))
  }

  # 3) Create virtualenv
  message("Creating virtualenv '", envname, "'...")
  reticulate::virtualenv_create(envname = envname)

  reticulate::use_virtualenv(envname, required = TRUE)

  # 4) Install packages
  message("Installing Python packages into '", envname, "' (virtualenv)...")
  reticulate::py_install(
    packages = c("sentence-transformers", "numpy"),
    envname  = envname,
    method   = "virtualenv"
  )

  message("Environment setup complete (virtualenv).")
  invisible(TRUE)
}
