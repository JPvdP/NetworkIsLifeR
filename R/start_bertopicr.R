#' Start the bertopicR Python environment
#'
#' This helper function configures \pkg{reticulate} to use a dedicated
#' Python virtual environment (created e.g. by \code{setup_bertopic_env()})
#' and optionally initializes spaCy via \pkg{spacyr}. It is intended as a
#' one-stop call at the beginning of an R session before using functions
#' that depend on Python (e.g. BERTopic-style topic modelling).
#'
#' The function is conservative: if Python has already been initialized in
#' the current R session, it will \emph{not} attempt to switch environments
#' (because \pkg{reticulate} does not support changing Python after
#' initialization). In that case it returns \code{FALSE} (invisibly) and,
#' if \code{verbose = TRUE}, prints a message indicating which Python is
#' currently in use.
#'
#' @param envname Character string giving the name of the virtual
#'   environment that should be used for \code{bertopicR}. This environment
#'   is expected to contain a working Python installation, \code{spaCy}, and
#'   any other required Python packages. Default is
#'   \code{"bertopic_r_env_spacy"}.
#' @param initialize_spacy Logical; if \code{TRUE} (default), attempt to
#'   initialize spaCy via \pkg{spacyr} using the specified Python
#'   environment. If \code{FALSE}, the function only configures
#'   \pkg{reticulate} (by setting \code{RETICULATE_PYTHON}) and does not
#'   call \code{spacyr::spacy_initialize()}.
#' @param verbose Logical; if \code{TRUE} (default), print informative
#'   messages about which Python binary is used, whether spaCy was
#'   initialized, and what to do if Python is already initialized.
#'
#' @details
#' Internally, the function:
#' \enumerate{
#'   \item Checks whether \pkg{reticulate} is available.
#'   \item If Python is already initialized (\code{reticulate::py_available(initialize = FALSE)}),
#'         it returns without making changes and advises the user to restart R
#'         if they want to switch environments.
#'   \item Uses \code{reticulate::virtualenv_python(envname)} to locate the
#'         Python executable inside the chosen virtual environment.
#'   \item Sets the \code{RETICULATE_PYTHON} environment variable to that
#'         path so that subsequent \pkg{reticulate} calls use this Python.
#'   \item Optionally calls \code{spacyr::spacy_initialize()} to initialize
#'         spaCy in that environment (if \pkg{spacyr} is installed and
#'         \code{initialize_spacy = TRUE}).
#' }
#'
#' The virtual environment referenced by \code{envname} must already exist
#' and contain the necessary Python packages. You can create and populate
#' such an environment with a companion function like
#' \code{setup_bertopic_env()}.
#'
#' @return Invisibly returns \code{TRUE} if the function successfully sets
#'   \code{RETICULATE_PYTHON} (and optionally initializes spaCy), or
#'   \code{FALSE} if Python was already initialized and no changes were
#'   made.
#'
#' @examples
#' \dontrun{
#' # 1. (Optional) Create and configure the environment first
#' # setup_bertopic_env(envname = "bertopic_r_env_spacy")
#'
#' # 2. Start bertopicR environment and initialize spaCy
#' start_bertopicr(envname = "bertopic_r_env_spacy",
#'                 initialize_spacy = TRUE,
#'                 verbose = TRUE)
#'
#' # 3. If you only want to configure reticulate without spaCy:
#' start_bertopicr(envname = "bertopic_r_env_spacy",
#'                 initialize_spacy = FALSE)
#' }
#'
#' @export
start_bertopicr <- function(envname = "bertopic_r_env_spacy",
                            initialize_spacy = TRUE,
                            verbose = TRUE) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required but not installed.")
  }

  # 1) If Python is already initialized, don't try to switch
  if (reticulate::py_available(initialize = FALSE)) {
    if (verbose) {
      cfg <- reticulate::py_config()
      message(
        "Python is already initialized at:\n  ", cfg$python,
        "\n\nIf this is not the environment you intended for bertopicR, ",
        "please restart R and call start_bertopicr() before any other code ",
        "that uses Python."
      )
    }
    return(invisible(FALSE))
  }

  # Helper: try to resolve envname to a Python binary
  resolve_python_path <- function(envname) {
    py <- NULL

    # Case A: envname looks like a path (contains / or \)
    if (grepl("[/\\\\]", envname)) {
      # 1) envname is already a python binary
      if (file.exists(envname)) {
        py <- normalizePath(envname)
      } else {
        # 2) env root: try Unix layout
        cand_unix <- file.path(envname, "bin", "python")
        # 3) env root: try Windows layout
        cand_win  <- file.path(envname, "Scripts", "python.exe")

        if (file.exists(cand_unix)) {
          py <- normalizePath(cand_unix)
        } else if (file.exists(cand_win)) {
          py <- normalizePath(cand_win)
        }
      }
      if (!is.null(py)) return(py)
    }

    # Case B: virtualenv by name
    py <- tryCatch(
      reticulate::virtualenv_python(envname),
      error = function(e) NULL
    )
    if (!is.null(py) && nzchar(py) && file.exists(py)) return(normalizePath(py))

    # Case C: conda env by name
    py <- tryCatch(
      reticulate::conda_python(envname),
      error = function(e) NULL
    )
    if (!is.null(py) && nzchar(py) && file.exists(py)) return(normalizePath(py))

    # Nothing worked
    NULL
  }

  py_path <- resolve_python_path(envname)

  if (is.null(py_path)) {
    # Give helpful diagnostics
    ve <- tryCatch(reticulate::virtualenv_list(), error = function(e) NULL)
    ce <- tryCatch(reticulate::conda_list(),       error = function(e) NULL)

    msg <- paste0(
      "Could not find a Python executable for '", envname, "'.\n",
      "- If this is a virtualenv name, check it appears in reticulate::virtualenv_list().\n",
      "- If this is a conda env name, check it appears in reticulate::conda_list().\n",
      "- If this is a path, make sure it points to either the env root or the python binary.\n"
    )

    if (!is.null(ve)) {
      msg <- paste0(
        msg,
        "\nVirtualenvs seen by reticulate::virtualenv_list():\n  ",
        paste(ve, collapse = ", ")
      )
    }

    if (!is.null(ce)) {
      msg <- paste0(
        msg,
        "\nConda envs seen by reticulate::conda_list():\n  ",
        paste(ce$name, collapse = ", ")
      )
    }

    stop(msg, call. = FALSE)
  }

  if (verbose) message("Using Python at: ", py_path)

  # Attach Python
  reticulate::use_python(py_path, required = TRUE)

  # Optionally initialise spacy via spacyr (if you use it)
  if (initialize_spacy) {
    if (!requireNamespace("spacyr", quietly = TRUE)) {
      warning("spacyr not installed; skipping spacy initialization.")
    } else {
      spacyr::spacy_initialize(
        python_executable = py_path,
        refresh_settings  = TRUE
      )
      if (verbose) message("spaCy initialised via spacyr.")
    }
  }

  invisible(TRUE)
}
