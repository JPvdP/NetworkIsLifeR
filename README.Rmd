# NetworkIsLifeR

**NetworkIsLifeR** is an R package built for Utrecht University data courses (Innometrics and Data Analytics for Sustainability), bundling practical utilities for:

- parsing Lens.org patent/publication exports (JSONL / JSONL.GZ),
- lightweight text/topic modelling workflows (BERTopic-style: embeddings → UMAP → HDBSCAN),
- topic representation (udpipe/quanteda or spaCy via Python),
- organization/company name cleaning & classification,
- small network helpers (igraph).

> Status: early development (version 0.0.1). Expect breaking changes.

---

## Installation

This package is not on CRAN. Install from GitHub:

```r
install.packages("remotes")
remotes::install_github("JPvdP/NetworkIsLifeR")