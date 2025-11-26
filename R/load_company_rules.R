#' load_company_rules
#'
#' This function loads the default company name cleaning rules that ship
#' with the package. The rules are stored in an external R script
#' (`company_rules.R`) located in the `inst/` directory of the package.
#' Users may edit this file after installation to customize the rule set.
#'
#' The file must define an object named `rules`, typically a tibble with
#' columns such as `priority`, `type`, `tokens`, `pattern`, and `canonical`.
#'
#' @param file Optional path to a custom rules file. If not supplied,
#'   the function loads the package's default rules from
#'   `inst/company_rules.R`.
#'
#' @return A tibble containing the rule set.
#'
#' @examples
#' \dontrun{
#' # Load the default rules included in the package
#' rules <- load_company_rules()
#'
#' # Load a user-modified rule file
#' custom_rules <- load_company_rules("my_rules.R")
#' }
#'
#' @export
load_company_rules <- function(file = NULL) {

  # If no custom file is given, load the package default
  if (is.null(file)) {
    file <- system.file("company_rules.R", package = utils::NetworkIsLifeR())

    if (file == "") {
      stop(
        "No rule file found. Expected 'inst/company_rules.R' in the package.\n",
        "Make sure the file exists and is included via `usethis::use_inst()`.",
        call. = FALSE
      )
    }
  }

  # Create a clean environment to avoid polluting the user's workspace
  env <- new.env(parent = emptyenv())

  # Source the rules file
  tryCatch(
    sys.source(file, envir = env),
    error = function(e) {
      stop("Failed to load rules from file '", file, "': ", e$message,
           call. = FALSE)
    }
  )

  # The rules must be present in the loaded environment
  if (!exists("rules", envir = env)) {
    stop(
      "The file '", file, "' does not define an object named `rules`.\n",
      "Make sure it contains a line like: rules <- tibble::tribble(...).",
      call. = FALSE
    )
  }

  env$rules
}
