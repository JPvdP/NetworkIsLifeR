#' clean_and_match_companies
#'
#' This function standardises company names, normalises legal forms, replaces
#' common long terms with short equivalents, sorts tokens alphabetically, and
#' performs adjacency-based fuzzy matching to identify potential duplicates.
#'
#' The goal is to clean noisy company names (from patents, publications, or
#' business registries) and detect likely matches using cosine similarity
#' between token-sorted representations of each name.
#'
#' @param df A data frame containing company names.
#' @param name_col A string specifying the column in \code{df} that contains
#'   company names to be cleaned and matched.
#' @param similarity_threshold Numeric value between 0 and 1. Two cleaned names
#'   are considered a match when their cosine similarity is greater than or
#'   equal to this threshold. Defaults to \code{0.85}.
#'
#' @details
#' The cleaning pipeline consists of the following steps:
#' \enumerate{
#'   \item Convert all names to lowercase.
#'   \item Remove Dutch and international legal forms (e.g., \code{BV}, \code{NV},
#'         \code{LLC}, \code{Ltd}, \code{GmbH}, \code{Stichting}).
#'   \item Replace common long words with shorter canonical forms (e.g.,
#'         \code{"technology" → "tech"}, \code{"university" → "univ"}).
#'   \item Remove punctuation and normalise whitespace.
#'   \item Split each name into tokens, alphabetically sort them, and recombine
#'         into a canonical cleaned representation.
#'   \item Order all names by the cleaned form and perform pairwise fuzzy
#'         matching on adjacent entries using cosine string similarity.
#' }
#'
#' This adjacency comparison is efficient and works well for large datasets
#' because token-sorted names that are similar will appear next to each other.
#'
#' @return
#' A data frame containing:
#' \describe{
#'   \item{Initial_name}{The original company name.}
#'   \item{Clean}{The cleaned, lowercased, normalised version.}
#'   \item{Clean2}{The token-sorted canonical name string.}
#'   \item{Matched}{The next adjacent company name (original form) whose
#'         cleaned version exceeds the similarity threshold, or \code{NA} if no
#'         similar neighbour was found.}
#' }
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   company = c(
#'     "Philips Research B.V.",
#'     "B.V. Philips Research",
#'     "Royal Philips",
#'     "Technische Universiteit Delft",
#'     "Delft University of Technology"
#'   )
#' )
#'
#' clean_and_match_companies(df, "company", similarity_threshold = 0.8)
#' }
#'
#' @note
#' Matching is adjacency-based after alphabetical token sorting. This is highly
#' efficient for large datasets, but if names are very different in structure,
#' non-adjacent matches may not be detected.
#'
#' @export
clean_and_match_companies <- function(df, name_col, similarity_threshold = 0.85) {

  library(dplyr)
  library(stringr)
  library(stringdist)

  #---------------------------#
  # 1. STANDARDISE COMPANY NAMES
  #---------------------------#

  # Dutch legal forms to remove completely
  legal_forms <- c("bv", "b\\.v\\."," b v", " n v"," bv" ," nv", "n\\.v\\.", "vof", "v\\.o\\.f\\.",
                   "cv", " llc", " ltd","coöperatie", "stichting", "vereniging", " inc$", " usa", " b u", "limited",
                   "gmbh", " sarl")
  #legal_regex <- paste0("\\b(", paste(legal_forms, collapse = "|"), ")\\b")
  legal_regex <- paste0("", paste(legal_forms, collapse = "|"), "")
  # Replacement dictionary: long → short forms
  replacements <- list(
    "technologies?" = "tech",
    "technology"    = "tech",
    "research"      = "tech",
    "management"    = "mgt",
    "solutions?"    = "sol",
    "consulting"    = "consult",
    "international" = "int",
    "services?"     = "serv",
    "holding"       = "hold",
    "group"         = "grp",
    "engineering"   = "eng",
    "holdings"      = "hold",
    "université"    = "univ",
    "university"    = "univ",
    "holdings"      = "hold",
    "comany"        = "co",
    "company"       = "co",
    "development"   = "dev"
  )

  df <- df %>%
    mutate(
      Initial_name = .data[[name_col]],
      Clean = stringr::str_to_lower(Initial_name),
      Clean = stringr::str_replace_all(Clean, legal_regex, " ")
    )
  df$Clean = stringr::str_replace_all(df$Clean, "[[:punct:]]", " ")
  df$Clean = stringr::str_replace_all(df$Clean, "-", " ")
  df$Clean = trimws(df$Clean)
  # Apply replacements (long → short forms)
  for (pattern in names(replacements)) {
    repl <- replacements[[pattern]]
    df$Clean <- stringr::str_replace_all(df$Clean, paste0("\\b", pattern, "\\b"), repl)
  }

  df <- df %>%
    mutate(
      Clean = stringr::str_squish(Clean)
    )

  #---------------------------#
  # 2. SORT TOKENS ALPHABETICALLY
  #---------------------------#
  df <- df %>%
    dplyr::mutate(
      tokens = stingr::str_split(Clean, " +"),
      Clean2  = sapply(tokens, function(x) paste(sort(x), collapse = " "))
    ) %>%
    dplyr::select(-tokens)

  #---------------------------#
  # 3. MATCH ADJACENT CLEANED NAMES
  #---------------------------#

  df <- df %>% dplyr::arrange(Clean2)
  matched <- rep(NA_character_, nrow(df))

  for (i in 1:(nrow(df)-1)) {
    name1 <- df$Clean2[i]
    name2 <- df$Clean2[i+1]

    sim <- stringdist::stringsim(name1, name2, method = "cosine")

    if (sim >= similarity_threshold) {
      matched[i] <- df$Initial_name[i+1]  # store original name
    }
  }

  df$Matched <- matched

  # Return final structure
  df %>% dplyr::select(Initial_name, Clean, Clean2, Matched)
}
