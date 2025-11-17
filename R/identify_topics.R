#' identify_topics
#'
#' This function performs the core preprocessing steps used in BERTopic:
#' sentence-transformer embeddings, UMAP dimensionality reduction, and
#' HDBSCAN clustering. It returns a dataframe mapping each input text to
#' a cluster label.
#'
#' @param texts Character vector of input documents.
#' @param model_name SentenceTransformer model name (default:
#'   "all-MiniLM-L6-v2").
#' @param n_neighbors Number of UMAP neighbors (default: 15).
#' @param n_components Number of UMAP dimensions (default: 10).
#' @param metric Distance metric for UMAP (default: "cosine").
#' @param minPts HDBSCAN minPts parameter (default: 10).
#' @importFrom dplyr filter group_by slice_max ungroup mutate
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_brewer coord_flip
#' @importFrom ggplot2 labs theme_minimal facet_wrap scale_x_discrete
#' @importFrom stats reorder
#' @importFrom grDevices colorRampPalette
#' @importFrom magrittr %>%
#' @examples
#' url <- "https://www.gutenberg.org/cache/epub/1342/pg1342.txt"
#' texts <- readr::read_lines(url)
#' # Keep only non-empty lines
#' texts <- texts[texts != ""]
#'
#' # Take a subset (e.g. first 300 lines)
#' texts <- texts[1:300]
#' plop <- identify_topics(texts)
#' @return A dataframe with doc_id, text, and cluster assignments.
#' @export
identify_topics <- function(
    texts,
    model_name    = "all-MiniLM-L6-v2",
    n_neighbors   = 15,
    n_components  = 10,
    metric        = "cosine",
    minPts        = 10
) {

  # ---- 1. Import Python modules ----
  sentence_transformers <- reticulate::import("sentence_transformers")
  np                    <- reticulate::import("numpy")

  # ---- 2. Compute embeddings ----
  message("Loading model: ", model_name, " ...")
  model <- sentence_transformers$SentenceTransformer(model_name)

  message("Encoding texts into embeddings...")
  emb <- model$encode(texts, show_progress_bar = TRUE)
  emb_np <- np$array(emb)
  emb_r  <- reticulate::py_to_r(emb_np)

  # ---- 3. UMAP dimensionality reduction ----
  message("Running UMAP reduction...")
  set.seed(42)
  embeddings_umap <- uwot::umap(
    emb_r,
    n_neighbors  = n_neighbors,
    n_components = n_components,
    metric       = metric
  )

  # ---- 4. HDBSCAN clustering ----
  message("Running HDBSCAN clustering...")
  clust <- dbscan::hdbscan(embeddings_umap, minPts = minPts)

  clusters <- clust$cluster

  # ---- 5. Combine into dataframe ----
  df <- data.frame(
    doc_id  = seq_along(texts),
    text    = texts,
    cluster = clusters
  )

  # Add embeddings/UMAP/clustering as attributes (optional)
  attr(df, "embeddings")      <- emb_r
  attr(df, "embeddings_umap") <- embeddings_umap
  attr(df, "hdbscan")         <- clust

  message("Finished! Returning dataframe with clusters.")

  return(df)
}
utils::globalVariables(c("topic", "score", "word"))
