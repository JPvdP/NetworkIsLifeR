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
ensure_bertopic_env <- function(
    envname = "bertopic_env",
    required_modules = c("numpy", "sentence_transformers"),
    verbose = TRUE
) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required but not installed.", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # 1. Find env: conda or virtualenv?
  # ---------------------------------------------------------------------------
  env_type <- NULL

  # Try conda environments
  conda_envs <- character()
  conda_envs <- tryCatch(
    {
      ce <- reticulate::conda_list()
      if (!is.null(ce$name)) ce$name else character()
    },
    error = function(e) character()
  )

  if (envname %in% conda_envs) {
    env_type <- "conda"
  }

  # Try virtualenv environments
  venvs <- character()
  venvs <- tryCatch(
    reticulate::virtualenv_list(),
    error = function(e) character()
  )

  if (envname %in% venvs) {
    if (!is.null(env_type) && env_type == "conda") {
      warning(
        "Environment '", envname,
        "' exists as both a conda env and a virtualenv. Using the conda env."
      )
    } else {
      env_type <- "virtualenv"
    }
  }

  if (is.null(env_type)) {
    stop(
      "Python environment '", envname, "' not found in conda or virtualenv.\n",
      "Create it first with your install function.",
      call. = FALSE
    )
  }

  if (verbose) {
    message("Using ", env_type, " environment: '", envname, "'.")
  }

  # ---------------------------------------------------------------------------
  # 2. Activate env BEFORE any Python initialization
  # ---------------------------------------------------------------------------
  if (env_type == "conda") {
    reticulate::use_condaenv(envname, required = TRUE)
  } else {
    reticulate::use_virtualenv(envname, required = TRUE)
  }

  # Optional safety: if Python is already initialized with another env
  if (reticulate::py_available(initialize = FALSE)) {
    cfg <- reticulate::py_config()
    if (verbose) {
      message("Python already initialized at: ", cfg$python)
    }
    # You *could* stop here if it's not the env you expect, but reticulate
    # should respect use_* calls made before the first py_* call.
  }

  # ---------------------------------------------------------------------------
  # 3. Check & install required Python modules
  # ---------------------------------------------------------------------------
  # mapping from Python import name -> pip/conda package name
  module_to_package <- function(mod) {
    switch(
      mod,
      "sentence_transformers" = "sentence-transformers",
      mod  # default: same name
    )
  }

  # helper to install a package into the correct env
  install_python_pkg <- function(package_name) {
    if (env_type == "conda") {
      # Using pip within the conda env is safest for sentence-transformers.
      reticulate::conda_install(
        envname,
        packages = package_name,
        pip = TRUE
      )
    } else {
      reticulate::virtualenv_install(
        envname,
        packages = package_name
      )
    }
  }

  for (mod in required_modules) {
    if (!reticulate::py_module_available(mod)) {
      pkg <- module_to_package(mod)
      if (verbose) {
        message("Python module '", mod, "' not available. Installing '", pkg, "'...")
      }
      install_python_pkg(pkg)

      # re-check after install
      if (!reticulate::py_module_available(mod)) {
        stop(
          "Failed to import Python module '", mod, "' after installing package '",
          pkg, "'. Check your Python environment and installation logs.",
          call. = FALSE
        )
      } else if (verbose) {
        message("Successfully installed and loaded module '", mod, "'.")
      }
    } else if (verbose) {
      message("Python module '", mod, "' is already available.")
    }
  }

  invisible(TRUE)
}
