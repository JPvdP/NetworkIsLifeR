# NetworkIsLifeR

`NetworkIsLifeR` is an R package built for Utrecht University data courses (Innometrics and Data Analytics for Sustainability).

## Aim

The package aims to make innovation-data workflows faster and more reproducible in R by providing ready-to-use helpers for:

- parsing Lens.org JSONL exports into tidy tables,
- extracting applicants, inventors, classifications, abstracts, and affiliations,
- cleaning and classifying organization/company names,
- building lightweight topic-modelling pipelines for exploratory analysis,
- supporting downstream network and bibliometric analysis.

In short: it reduces boilerplate so you can move from raw export files to analysis-ready data.

## What The Package Covers

- **Patent/publication ingestion**: `process_patent_jsonl()`, `process_patent_jsonl_fast()`, `list_lens_files()`
- **Entity extraction**: `extract_applicants_table()`, `extract_inventors_table()`, `extract_classifications_table()`, `extract_classifications_table_CPC()`
- **Data cleaning/classification**: `clean_company_names()`, `clean_and_match_companies()`, `classify_organization()`, `split_scopus_affiliations()`
- **Topic modelling helpers**: `identify_topics()`, `compute_topic_terms_udpipe()`, `compute_topic_tf_idf_spacy_py()`, plotting helpers
- **Network helper**: `extract_giant_component()`

> Status: early development (`0.0.1`). Interfaces may still change.

---

## Installation

This package is not on CRAN. Install from GitHub:

```r
install.packages("remotes")
remotes::install_github("JPvdP/NetworkIsLifeR")
```

## Examples

### 1) Parse a Lens JSONL file into a tidy patent table

```r
library(NetworkIsLifeR)

example_file <- system.file("extdata", "lens-export-3.jsonl", package = "NetworkIsLifeR")

patents <- process_patent_jsonl(
  file_path = example_file,
  max_records = 100,
  verbose = FALSE
)

head(patents)
```

### 2) Extract applicants and assign one country per assignee

```r
library(dplyr)
library(NetworkIsLifeR)

example_file <- system.file("extdata", "lens-export-3.jsonl", package = "NetworkIsLifeR")

apps <- extract_applicants_table(
  file_path = example_file,
  max_records = 200,
  verbose = FALSE
)

assignee_country <- extract_assignee_country_unique(
  apps,
  name_col = "name",
  residence_col = "residence",
  address_col = "address"
)

assignee_country %>%
  arrange(desc(total_records)) %>%
  head(10)
```

### 3) Split Scopus affiliations and classify organizations

```r
library(NetworkIsLifeR)

scopus <- data.frame(
  EID = c("2-s2.0-1", "2-s2.0-2"),
  Affiliations = c(
    "Utrecht University, Utrecht, Netherlands; Siemens AG, Munich, Germany",
    "Ministry of Health, Madrid, Spain; University of Oxford, Oxford, United Kingdom"
  )
)

affiliations_long <- split_scopus_affiliations(scopus)
classified <- classify_organization(affiliations_long, affiliation_name)

classified[, c("affiliation_name", "country", "org_type", "confidence")]
```
