#' Ensure Python environment for BERTopic exists and has required packages
#'
#' This function:
#' 1. Detects whether `envname` is a conda or virtualenv environment.
#' 2. Activates it via reticulate.
#' 3. Ensures that numpy and sentence-transformers are installed.
#'
#' @param envname Name of the Python environment to use.
#' @param required_modules Python modules that must be importable.
#'        Defaults to c("numpy", "sentence_transformers").
#' @param verbose Logical; print messages?
#'
#' @return Invisibly TRUE on success, otherwise stops with an error.
#' @keywords internal
setup_bertopic_env <- function(envname = "bertopic_r_env",
                               python_version = "3.11",
                               use_conda_on_windows = TRUE) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required but not installed.")
  }

  is_windows <- identical(.Platform$OS.type, "windows")

  # Helper: find a system Python on non-Windows
  find_system_python <- function() {
    for (exe in c("python3", "python")) {
      p <- Sys.which(exe)
      if (nzchar(p)) return(unname(p))
    }
    ""
  }

  ## ------------------------------------------------------------------
  ## WINDOWS: use conda / Miniconda
  ## ------------------------------------------------------------------
  if (is_windows && use_conda_on_windows) {

    # 1) Find or install Miniconda
    conda_bin <- tryCatch(
      reticulate::conda_binary(),
      error = function(e) ""
    )

    if (!nzchar(conda_bin)) {
      message("No conda found. Installing Miniconda for reticulate...")
      reticulate::install_miniconda()

      conda_bin <- tryCatch(
        reticulate::conda_binary(),
        error = function(e) ""
      )
      if (!nzchar(conda_bin)) {
        stop("Could not find Miniconda / conda binary even after install.")
      }
    }

    # 2) Create env if needed
    existing_envs <- tryCatch(
      reticulate::conda_list()$name,
      error = function(e) character()
    )

    if (!(envname %in% existing_envs)) {
      if (interactive()) {
        cat(
          "To create a conda environment, conda may use the Anaconda default channels,\n",
          "which are protected by Terms of Service:\n",
          "  https://www.anaconda.com/legal/terms-of-service\n\n",
          "By confirming below, you indicate that you have read and agree to these terms.\n\n",
          sep = ""
        )
        ans <- readline("Do you confirm that you have read and agree to these Terms of Service? [yes/no]: ")
        ans <- tolower(trimws(ans))
        if (!ans %in% c("yes", "y")) {
          stop("You did not accept the Terms of Service. Cannot proceed with conda-based setup.")
        }
        # Let the ToS plugin auto-accept in this session
        Sys.setenv(CONDA_PLUGINS_AUTO_ACCEPT_TOS = "yes")
      } else {
        stop(
          "Conda may require accepting Anaconda's Terms of Service, but this R session is non-interactive.\n",
          "Please run setup_bertopic_env() interactively once, or accept the ToS manually."
        )
      }

      message("Creating conda environment '", envname,
              "' with Python ", python_version, " ...")
      reticulate::conda_create(
        envname  = envname,
        packages = paste0("python=", python_version)
        # optionally: channel = "conda-forge"
      )
    } else {
      message("Conda environment '", envname, "' already exists.")
    }

    # 3) Use that env in this R session
    reticulate::use_condaenv(envname, required = TRUE)

    # 4) Install required Python packages
    message("Installing Python packages into '", envname, "' (conda)...")
    reticulate::py_install(
      packages = c("sentence-transformers", "numpy"),
      envname  = envname,
      method   = "conda"
    )

    # Optionally store for later use in other functions
    options(bertopic_r.env = list(type = "conda", name = envname))

    message("Environment setup complete (conda, Windows).")
    return(invisible(TRUE))
  }

  ## ------------------------------------------------------------------
  ## macOS / Linux / Windows without conda: virtualenv
  ## ------------------------------------------------------------------

  py_exec <- find_system_python()

  if (!nzchar(py_exec)) {
    stop(
      "No Python executable ('python3' or 'python') found on PATH.\n",
      "Please install Python ", python_version,
      " (e.g., from python.org, Homebrew, or your package manager),\n",
      "then run setup_bertopic_env() again."
    )
  }

  existing_envs <- tryCatch(
    reticulate::virtualenv_list(),
    error = function(e) character()
  )

  if (!(envname %in% existing_envs)) {
    message("Creating virtualenv '", envname,
            "' using Python at:\n  ", py_exec)
    reticulate::virtualenv_create(envname = envname, python = py_exec)
  } else {
    message("Virtual environment '", envname, "' already exists.")
  }

  # Use this env in the current R session
  reticulate::use_virtualenv(envname, required = TRUE)

  # Install required packages
  message("Installing Python packages into '", envname, "' (virtualenv)...")
  reticulate::py_install(
    packages = c("sentence-transformers", "numpy"),
    envname  = envname,
    method   = "virtualenv"
  )

  options(bertopic_r.env = list(type = "virtualenv", name = envname))

  message("Environment setup complete (virtualenv).")
  invisible(TRUE)
}
