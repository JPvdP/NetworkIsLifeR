#' Identify topics & export coordinates2
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
identify_topics_coord2 <- function(
    texts,
    model_name           = "all-MiniLM-L6-v2",
    n_neighbors          = 15,
    n_components         = 10,
    metric               = "cosine",
    minPts               = 10,
    assign_by_membership = FALSE,

    # NEW: allow passing a table/vector of topic labels (shown in legend, not drawn on the map)
    topic_labels         = NULL,   # data.frame or named vector
    topic_id_col         = "cluster",
    topic_label_col      = "label",

    # NEW: optional plot
    return_plot          = FALSE,
    plot_dims            = c(1, 2),
    point_size           = 1.2,
    point_alpha          = 0.8
) {

  # ---- 0. Normalize input (character vector OR data.frame/tibble with a text column) ----
  if (is.data.frame(texts)) {
    if (!("text" %in% names(texts))) {
      stop("If `texts` is a data.frame, it must contain a column named `text` (or adapt this function to your column name).")
    }
    texts_vec <- as.character(texts$text)
    meta_df   <- texts
  } else {
    texts_vec <- as.character(texts)
    meta_df   <- NULL
  }

  n_docs <- length(texts_vec)
  if (n_docs == 0) stop("No texts provided.")

  # ---- 1. Import Python modules ----
  sentence_transformers <- reticulate::import("sentence_transformers")
  np                    <- reticulate::import("numpy")

  # ---- 2. Compute embeddings ----
  message("Loading model: ", model_name, " ...")
  model <- sentence_transformers$SentenceTransformer(model_name)

  message("Encoding texts into embeddings...")
  emb    <- model$encode(texts_vec, show_progress_bar = TRUE)
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
  clusters <- clust$cluster  # 0 = noise

  # ---- 4b. Optional: assign every doc to its most probable topic ----
  if (assign_by_membership) {
    if (is.null(clust$membership)) {
      warning("assign_by_membership = TRUE, but no membership matrix found. Using original clusters.")
    } else {
      mem <- clust$membership
      clusters <- apply(mem, 1, which.max)
      message("assign_by_membership = TRUE: all documents assigned to their most probable topic; no noise cluster remains.")
    }
  }

  # ---- 5. Build cluster labels (for legend) without plotting text on the map ----
  make_cluster_label <- function(clusters, topic_labels, topic_id_col, topic_label_col) {
    cluster_chr <- as.character(clusters)

    if (is.null(topic_labels)) {
      lab <- cluster_chr
      return(factor(lab, levels = sort(unique(lab))))
    }

    if (is.data.frame(topic_labels)) {
      if (!all(c(topic_id_col, topic_label_col) %in% names(topic_labels))) {
        stop("`topic_labels` data.frame must contain columns: `", topic_id_col, "` and `", topic_label_col, "`.")
      }
      lab_map <- setNames(
        as.character(topic_labels[[topic_label_col]]),
        as.character(topic_labels[[topic_id_col]])
      )
    } else if (is.vector(topic_labels) && !is.list(topic_labels)) {
      if (is.null(names(topic_labels))) {
        stop("If `topic_labels` is a vector, it must be a *named* vector with names = cluster ids.")
      }
      lab_map <- setNames(as.character(topic_labels), as.character(names(topic_labels)))
    } else {
      stop("`topic_labels` must be either a data.frame or a named vector.")
    }

    lab <- ifelse(cluster_chr %in% names(lab_map), lab_map[cluster_chr], cluster_chr)

    # keep legend order stable: by first appearance in the data
    factor(lab, levels = unique(lab))
  }

  cluster_label <- make_cluster_label(clusters, topic_labels, topic_id_col, topic_label_col)

  # ---- 6. Main document dataframe ----
  if (is.null(meta_df)) {
    df_docs <- data.frame(
      doc_id        = seq_len(n_docs),
      text          = texts_vec,
      cluster       = clusters,
      cluster_label = cluster_label,
      stringsAsFactors = FALSE
    )
  } else {
    df_docs <- meta_df
    df_docs$doc_id        <- seq_len(n_docs)
    df_docs$cluster       <- clusters
    df_docs$cluster_label <- cluster_label
    # reorder a bit (optional)
    df_docs <- df_docs[, c("doc_id", setdiff(names(df_docs), "doc_id")), drop = FALSE]
  }

  # ---- 7. Coordinates dataframe for visualisation ----
  coord_names <- paste0("dim_", seq_len(ncol(embeddings_umap)))

  df_coords <- as.data.frame(embeddings_umap)
  colnames(df_coords) <- coord_names

  df_coords <- data.frame(
    doc_id        = seq_len(n_docs),
    df_coords,
    cluster       = clusters,
    cluster_label = cluster_label,
    stringsAsFactors = FALSE
  )

  # ---- 8. Attach extra objects as attributes ----
  attr(df_docs, "embeddings")      <- emb_r
  attr(df_docs, "embeddings_umap") <- embeddings_umap
  attr(df_docs, "hdbscan")         <- clust

  # ---- 9. Optional plot: labels ONLY in legend (no text drawn on the map) ----
  umap_plot <- NULL
  if (isTRUE(return_plot)) {
    if (length(plot_dims) != 2) stop("`plot_dims` must be length 2, e.g. c(1,2).")
    xcol <- paste0("dim_", plot_dims[1])
    ycol <- paste0("dim_", plot_dims[2])
    if (!all(c(xcol, ycol) %in% names(df_coords))) {
      stop("Requested `plot_dims` not available. You have dims: 1..", ncol(embeddings_umap))
    }

    umap_plot <- ggplot2::ggplot(
      df_coords,
      ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]], colour = cluster_label)
    ) +
      ggplot2::geom_point(size = point_size, alpha = point_alpha) +
      ggplot2::labs(x = xcol, y = ycol, colour = "Topic") +
      ggplot2::theme_minimal()

    # NOTE: intentionally NO geom_text/geom_label here.
    # Labels appear only in the legend via `colour = cluster_label`.
  }

  message("Finished! Returning documents and coordinates.")

  out <- list(
    documents       = df_docs,
    doc_coordinates = df_coords
  )
  if (isTRUE(return_plot)) out$plot <- umap_plot

  out
}
