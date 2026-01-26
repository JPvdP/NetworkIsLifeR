#' Clean company names using rule-based canonicalization
#'
#' Applies a prioritized set of string/regex matching rules to map company names
#' to canonical names. The original company names are always preserved.
#'
#' A rule matches if:
#' - `pattern` is non-NA and `grepl(pattern, x)` is TRUE, OR
#' - `pattern` is NA and `tokens` is used:
#'   - type == "ALL": all token regexes must match
#'   - type == "ANY": at least one token regex must match
#'
#' Rules are applied in ascending `priority` order; the first match wins.
#'
#' @param df A data.frame/tibble containing company names.
#' @param company_col Character scalar. Column name in `df` with company names.
#' @param rules Optional rule tibble/data.frame with columns:
#'   `priority`, `type`, `tokens` (list-column), `pattern`, `canonical`.
#'   If NULL, uses the package ruleset from `company_rules.r` (see Details).
#' @param overwrite Logical. If TRUE, overwrites `company_col` with cleaned names.
#'   The original is still preserved in `original_col`.
#' @param original_col Character scalar. Column name to store original names.
#' @param cleaned_col Character scalar. Column name to store cleaned names when
#'   `overwrite = FALSE`.
#' @param ignore_case Logical. If TRUE, matching is case-insensitive.
#' @param trim Logical. If TRUE, trims whitespace and collapses multiple spaces
#'   in both original and cleaned values (recommended).
#' @param return_rule Logical. If TRUE, adds a column `rule_index` indicating
#'   which rule row (after ordering) matched each name (NA if none).
#'
#' @return `df` with original column preserved and cleaned names added/updated.
#' @export
clean_company_names <- function(df,
                                company_col   = "company",
                                rules         = NULL,
                                overwrite     = FALSE,
                                original_col  = paste0(company_col, "_raw"),
                                cleaned_col   = paste0(company_col, "_clean"),
                                ignore_case   = TRUE,
                                trim          = TRUE,
                                return_rule   = FALSE) {
  library(tidyverse)
  if (!is.data.frame(df)) stop("`df` must be a data.frame or tibble.")
  if (!is.character(company_col) || length(company_col) != 1) {
    stop("`company_col` must be a single character string.")
  }
  if (!company_col %in% names(df)) {
    stop("`df` does not contain column `", company_col, "`.")
  }

  if (is.null(rules)) rules <- load_company_rules()

  # Validate rules -----------------------------------------------------------
  req_cols <- c("priority", "type", "tokens", "pattern", "canonical")
  miss <- setdiff(req_cols, names(rules))
  if (length(miss) > 0) {
    stop("`rules` is missing required columns: ", paste(miss, collapse = ", "))
  }

  # Coerce + order rules
  rules <- as.data.frame(rules, stringsAsFactors = FALSE)
  if (!is.list(rules$tokens)) {
    stop("`rules$tokens` must be a list-column (each element a character vector).")
  }
  rules$type <- as.character(rules$type)
  rules$canonical <- as.character(rules$canonical)

  # Order by priority (ascending). Stable tie-breaker: original row order.
  ord <- order(rules$priority, seq_len(nrow(rules)))
  rules <- rules[ord, , drop = FALSE]
  rownames(rules) <- NULL

  # Pull names
  x <- df[[company_col]]
  x_chr <- as.character(x)

  if (trim) {
    x_chr <- .collapse_spaces(x_chr)
  }

  # Always preserve original ------------------------------------------------
  if (!original_col %in% names(df)) {
    df[[original_col]] <- x_chr
  }

  # Apply rules -------------------------------------------------------------
  n <- length(x_chr)
  matched_canonical <- rep(NA_character_, n)
  matched_rule_idx  <- rep(NA_integer_, n)

  remaining <- which(!is.na(x_chr) & nzchar(x_chr))

  for (i in seq_len(nrow(rules))) {
    if (length(remaining) == 0) break

    hit <- .rule_matches(
      x_chr[remaining],
      type        = rules$type[i],
      tokens      = rules$tokens[[i]],
      pattern     = rules$pattern[i],
      ignore_case = ignore_case
    )

    if (any(hit)) {
      idx <- remaining[hit]
      matched_canonical[idx] <- rules$canonical[i]
      matched_rule_idx[idx]  <- i
      remaining <- setdiff(remaining, idx)
    }
  }

  cleaned <- ifelse(!is.na(matched_canonical), matched_canonical, x_chr)
  if (trim) cleaned <- .collapse_spaces(cleaned)

  if (overwrite) {
    df[[company_col]] <- cleaned
  } else {
    df[[cleaned_col]] <- cleaned
  }

  if (return_rule) {
    df[["rule_index"]] <- matched_rule_idx
  }

  df
}


.rule_matches <- function(x, type, tokens, pattern, ignore_case = TRUE) {
  # x: character vector
  # type: "ALL" / "ANY" (others can be added)
  # tokens: character vector (regexes) or NULL
  # pattern: single regex or NA

  # Prefer explicit pattern if supplied
  if (!all(is.na(pattern))) {
    pat <- as.character(pattern)[1]
    return(grepl(pat, x, ignore.case = ignore_case, perl = TRUE))
  }

  # Fall back to tokens
  if (is.null(tokens) || length(tokens) == 0 || all(is.na(tokens))) {
    return(rep(FALSE, length(x)))
  }

  tokens <- as.character(tokens)
  type <- toupper(as.character(type)[1])

  if (type == "ALL") {
    out <- rep(TRUE, length(x))
    for (t in tokens) {
      out <- out & grepl(t, x, ignore.case = ignore_case, perl = TRUE)
    }
    return(out)
  }

  if (type == "ANY") {
    out <- rep(FALSE, length(x))
    for (t in tokens) {
      out <- out | grepl(t, x, ignore.case = ignore_case, perl = TRUE)
    }
    return(out)
  }

  stop("Unsupported rule `type`: ", type, " (supported: 'ALL', 'ANY').")
}

.collapse_spaces <- function(x) {
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  x <- trimws(x)
  x
}
