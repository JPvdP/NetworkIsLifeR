# Load required packages
library(jsonlite)
library(dplyr)
library(purrr)

# ---- CONFIG ----
# Path to your Lens.org JSONL file
jsonl_file <- "lens-export-3.jsonl"
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)


# Helper to safely extract values (returns NA if missing)
safe_extract <- function(expr) {
  tryCatch(expr, error = function(e) NA, warning = function(w) NA)
}

# Function to parse one record safely
parse_lens_record <- function(line) {
  # Skip empty lines
  if (is.na(line) || str_trim(line) == "") return(NULL)

  # Parse JSON safely
  x <- tryCatch(fromJSON(line, simplifyVector = TRUE), error = function(e) {
    message("⚠️ Failed to parse JSON line: ", substr(line, 1, 80))
    return(NULL)
  })
  if (is.null(x)) return(NULL)

  # Extract fields (many may be missing)
  tibble(
    lens_id        = safe_extract(x$lens_id),
    jurisdiction   = safe_extract(x$jurisdiction),
    doc_number     = safe_extract(x$doc_number),
    kind           = safe_extract(x$kind),
    date_published = safe_extract(x$date_published),
    lang           = safe_extract(x$lang),
    title_en       = safe_extract({
      titles <- x$biblio$invention_title
      titles[titles$lang == "en", "text", drop = TRUE][1]
    }),
    applicants = safe_extract({
      apps <- x$biblio$parties$applicants
      if (is.null(apps) || length(apps) == 0) return(NA_character_)

      # Try all possible name fields
      names <- map_chr(apps, function(a) {
        if (!is.null(a$extracted_name$value)) return(a$extracted_name$value)
        if (!is.null(a$name)) return(a$name)
        if (!is.null(a$organization_name)) return(a$organization_name)
        return(NA_character_)
      })
      paste(na.omit(names), collapse = "; ")
    }),

    inventors = safe_extract({
      invs <- x$biblio$parties$inventors
      if (is.null(invs) || length(invs) == 0) return(NA_character_)

      names <- map_chr(invs, function(i) {
        if (!is.null(i$extracted_name$value)) return(i$extracted_name$value)
        if (!is.null(i$name)) return(i$name)
        return(NA_character_)
      })
      paste(na.omit(names), collapse = "; ")
    }),

    ipc_classes    = safe_extract({
      paste(map_chr(x$biblio$classifications_ipcr$classifications, ~ .x$symbol), collapse = "; ")
    }),
    abstract_en    = safe_extract({
      abstracts <- x$abstract
      abstracts[abstracts$lang == "en", "text", drop = TRUE][1]
    }),
    legal_status   = safe_extract(x$legal_status$patent_status),
    grant_date     = safe_extract(x$legal_status$grant_date),
    expiry_date    = safe_extract(x$legal_status$application_expiry_date)
  )
}

# ---- STREAMING READER ----
con <- file(jsonl_file, "r")
results <- list()
i <- 1
while (TRUE) {
  line <- readLines(con, n = 1, warn = FALSE)
  if (length(line) == 0) break
  rec <- parse_lens_record(line)
  if (!is.null(rec)) results[[i]] <- rec
  if (i %% 1000 == 0) message("Parsed ", i, " records...")
  i <- i + 1
}
close(con)

lens_df <- bind_rows(results)
print(head(lens_df))
