#' extract_patent_info
#'
#' @keywords internal
#' @noRd
extract_patent_info <- function(patent_json) {
# Get application reference - handle both direct access and nested structure
app_ref <- patent_json$biblio$application_reference
app_doc <- NA
app_date <- NA
if (!is.null(app_ref)) {
  app_doc <- if(!is.null(app_ref$doc_number)) app_ref$doc_number else NA
  app_date <- if(!is.null(app_ref$date)) app_ref$date else NA
}

# Get priority claims - handle complex nesting
priority_claims <- patent_json$biblio$priority_claims
priority_date <- NA
if (!is.null(priority_claims)) {
  # Try earliest_claim first
  if (!is.null(priority_claims$earliest_claim)) {
    priority_date <- priority_claims$earliest_claim$date
  }
  # If that fails, try first claim in claims array
  if (is.na(priority_date) && !is.null(priority_claims$claims)) {
    if (length(priority_claims$claims) > 0) {
      priority_date <- priority_claims$claims[[1]]$date
    }
  }
}

# Extract title (prefer English)
title <- NA
titles <- patent_json$biblio$invention_title
if (!is.null(titles) && length(titles) > 0) {
  if (is.list(titles) && !is.null(names(titles))) {
    # Single title object
    title <- titles$text
  } else {
    # Array of titles
    for (t in titles) {
      if (!is.null(t$lang) && t$lang == "en" && !is.null(t$text)) {
        title <- t$text
        break
      }
    }
    # If no English title found, take first available
    if (is.na(title) && !is.null(titles[[1]]$text)) {
      title <- titles[[1]]$text
    }
  }
}

# Extract abstract (prefer English)
abstract <- NA
abstracts <- patent_json$abstract
if (!is.null(abstracts) && length(abstracts) > 0) {
  if (is.list(abstracts) && !is.null(names(abstracts))) {
    # Single abstract object
    abstract <- abstracts$text
  } else {
    # Array of abstracts
    for (a in abstracts) {
      if (!is.null(a$lang) && a$lang == "en" && !is.null(a$text)) {
        abstract <- a$text
        break
      }
    }
    # If no English abstract found, take first available
    if (is.na(abstract) && !is.null(abstracts[[1]]$text)) {
      abstract <- abstracts[[1]]$text
    }
  }
}

tibble::tibble(
  # Basic identifiers
  lens_id = if(!is.null(patent_json$lens_id)) patent_json$lens_id else NA,
  jurisdiction = if(!is.null(patent_json$jurisdiction)) patent_json$jurisdiction else NA,
  doc_number = if(!is.null(patent_json$doc_number)) patent_json$doc_number else NA,
  kind = if(!is.null(patent_json$kind)) patent_json$kind else NA,
  date_published = if(!is.null(patent_json$date_published)) patent_json$date_published else NA,
  lang = if(!is.null(patent_json$lang)) patent_json$lang else NA,

  # Title and abstract
  title = title,
  abstract = abstract,

  # Application info
  app_doc_number = app_doc,
  app_date = app_date,

  # Priority date
  priority_date = priority_date,

  # Parties (all applicants and inventors, separated by semicolons)
  applicants = get_all_applicants(patent_json),
  inventors = get_all_inventors(patent_json),

  # All IPC Classifications (separated by semicolons)
  ipc_classifications = get_all_classifications(patent_json),

  # Legal status
  patent_status = safe_extract(patent_json, "legal_status", "patent_status"),
  granted = safe_extract(patent_json, "legal_status", "granted"),
  grant_date = safe_extract(patent_json, "legal_status", "grant_date"),

  # Citation count
  cited_by_count = {
    cited <- patent_json$biblio$cited_by$patent_count
    if (!is.null(cited)) cited else 0
  },

  # Family size
  simple_family_size = safe_extract(patent_json, "families", "simple_family", "size"),
  extended_family_size = safe_extract(patent_json, "families", "extended_family", "size")
)
}
