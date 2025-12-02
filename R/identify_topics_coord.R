#' Identify topics & export coordinates
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
#' #plop <- identify_topics(texts)
#' @return A dataframe with doc_id, text, and cluster assignments.
#' @export
identify_topics_coord <- function(
    texts,
    model_name           = "all-MiniLM-L6-v2",
    n_neighbors          = 15,
    n_components         = 10,
    metric               = "cosine",
    minPts               = 10,
    assign_by_membership = FALSE  # NEW: force assignment to the most probable topic
) {

  # ---- 1. Import Python modules ----
  sentence_transformers <- reticulate::import("sentence_transformers")
  np                    <- reticulate::import("numpy")

  # ---- 2. Compute embeddings ----
  message("Loading model: ", model_name, " ...")
  model <- sentence_transformers$SentenceTransformer(model_name)

  message("Encoding texts into embeddings...")
  emb    <- model$encode(texts, show_progress_bar = TRUE)
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

  # Original hard clusters from HDBSCAN (0 = noise in dbscan::hdbscan)
  clusters <- clust$cluster

  # ---- 4b. Optional: assign every doc to its most probable topic ----
  if (assign_by_membership) {
    if (is.null(clust$membership)) {
      warning("assign_by_membership = TRUE, but no membership matrix found in HDBSCAN result. Using original clusters.")
    } else {
      # membership: n_docs x K matrix, probabilities per topic
      mem <- clust$membership

      # For each document, choose the topic with highest membership probability.
      # This yields cluster labels in 1:K, i.e. no noise cluster (0 / -1) anymore.
      new_clusters <- apply(mem, 1, which.max)

      # Replace clusters with membership-based assignment
      clusters <- new_clusters

      message("assign_by_membership = TRUE: all documents assigned to their most probable topic; no noise cluster remains.")
    }
  }

  # ---- 5. Main document dataframe ----
  df_docs <- data.frame(
    doc_id  = seq_along(texts),
    text    = texts,
    cluster = clusters
  )

  # ---- 6. Coordinates dataframe for visualisation ----
  coord_names <- paste0("dim_", seq_len(ncol(embeddings_umap)))

  df_coords <- as.data.frame(embeddings_umap)
  colnames(df_coords) <- coord_names

  df_coords <- data.frame(
    doc_id  = seq_along(texts),
    df_coords,
    cluster = clusters
  )

  # ---- 7. Attach extra objects as attributes (optional) ----
  # clust still contains the original HDBSCAN result, including:
  # - clust$cluster  (with noise)
  # - clust$membership (soft memberships)
  attr(df_docs, "embeddings")      <- emb_r
  attr(df_docs, "embeddings_umap") <- embeddings_umap
  attr(df_docs, "hdbscan")         <- clust

  message("Finished! Returning documents and coordinates.")

  # ---- 8. Return both ----
  list(
    documents       = df_docs,
    doc_coordinates = df_coords
  )
}
