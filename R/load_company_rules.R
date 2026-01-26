#' Load company name cleaning rules from an .RData/.rda file
#'
#' Loads a rules table (data.frame/tibble) containing company name cleaning and
#' harmonisation rules. By default, the function reads an installed rules file
#' shipped with the package (e.g., \code{inst/company_rules.rdata}). The loaded
#' object is searched by name (see \code{object_names}); if no named object is
#' found, the function auto-detects the first data.frame that contains the
#' required columns.
#'
#' The rules table must contain at least the following columns:
#' \code{priority}, \code{type}, \code{tokens}, \code{pattern}, \code{canonical}.
#' The \code{tokens} column must be a list-column (list of character vectors).
#'
#' @param file Optional path to a \code{.RData}/\code{.rda} file containing a rules
#'   object. If \code{NULL}, the function loads \code{default_inst_file} from the
#'   installed \code{package}.
#' @param package Package name to search for the default installed rules file
#'   when \code{file = NULL}.
#' @param default_inst_file Filename expected in \code{inst/} of the package and
#'   retrievable via \code{\link[base]{system.file}}.
#' @param object_names Character vector of preferred object names to search for
#'   inside the loaded file (first match is used).
#'
#' @return A rules table as a data.frame or tibble (if \pkg{tibble} is installed),
#'   with validated structure and defensively normalised column types.
#'
#' @examples
#' # Load the package default rules (shipped in inst/)
#' rules <- load_company_rules()
#'
#' # Load rules from a custom file
#' # rules <- load_company_rules(file = "path/to/company_rules.rdata")
#'
#' @export
load_company_rules <- function(file = NULL,
                               package = "NetworkIsLifeR",
                               default_inst_file = "company_rules.rdata",
                               object_names = c("rules", "company_rules")) {
  # Resolve file path ---------------------------------------------------------
  if (is.null(file)) {
    file <- system.file(default_inst_file, package = package)

    if (file == "" || !file.exists(file)) {
      stop(
        "Default rules file not found in installed package.\n",
        "Expected: inst/", default_inst_file, "\n",
        "Make sure it is included in the package and reinstalled.\n",
        call. = FALSE
      )
    }
  } else {
    if (!file.exists(file)) {
      stop("Supplied file does not exist: ", file, call. = FALSE)
    }
  }

  # Load .RData/.rda into a clean environment --------------------------------
  ext <- tolower(tools::file_ext(file))
  if (!ext %in% c("rdata", "rda")) {
    stop("Unsupported file type: .", ext, "\nExpected a .RData/.rda file.", call. = FALSE)
  }

  env <- new.env(parent = baseenv())
  loaded_names <- load(file, envir = env)

  required_cols <- c("priority", "type", "tokens", "pattern", "canonical")

  # 1) Prefer explicit object names
  rules <- NULL
  for (nm in object_names) {
    if (exists(nm, envir = env, inherits = FALSE)) {
      rules <- get(nm, envir = env, inherits = FALSE)
      break
    }
  }

  # 2) Otherwise auto-detect a suitable object
  if (is.null(rules)) {
    candidates <- mget(loaded_names, envir = env, inherits = FALSE)
    ok <- vapply(
      candidates,
      function(x) is.data.frame(x) && all(required_cols %in% names(x)),
      logical(1)
    )
    if (!any(ok)) {
      stop(
        "No suitable rules object found in: ", file, "\n",
        "Expected an object named one of: ",
        paste(object_names, collapse = ", "),
        "\nOr any data.frame with columns: ",
        paste(required_cols, collapse = ", "),
        call. = FALSE
      )
    }
    rules <- candidates[[which(ok)[1]]]
  }

  # Validate structure --------------------------------------------------------
  if (!is.data.frame(rules)) {
    stop("Loaded object is not a data.frame/tibble.", call. = FALSE)
  }

  missing_cols <- setdiff(required_cols, names(rules))
  if (length(missing_cols) > 0) {
    stop("Rules object is missing required columns: ",
         paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  if (!is.list(rules$tokens)) {
    stop("Column `tokens` must be a list-column (list of character vectors).", call. = FALSE)
  }

  # Defensive type normalization
  rules$priority  <- suppressWarnings(as.integer(rules$priority))
  rules$type      <- as.character(rules$type)
  rules$pattern   <- ifelse(is.na(rules$pattern), NA_character_, as.character(rules$pattern))
  rules$canonical <- as.character(rules$canonical)

  if (requireNamespace("tibble", quietly = TRUE)) {
    rules <- tibble::as_tibble(rules)
  }

  rules
}
