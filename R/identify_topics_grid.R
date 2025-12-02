#' Grid Search for Topic Modelling Parameters
#'
#' \code{identify_topics_grid()} performs a grid search over multiple
#' topic-modelling parameters, allowing the user to explore the effect of
#' different models, UMAP settings, and HDBSCAN clustering thresholds.
#' Instead of computing embeddings for every parameter combination, the
#' function computes embeddings \emph{once per model} and reuses them,
#' making the grid search efficient even for large text datasets.
#'
#' For each parameter combination, the function returns:
#' \itemize{
#'   \item the number of detected topics,
#'   \item the number of outlier documents,
#'   \item the percentage of outliers.
#' }
#'
#' @param texts A character vector containing the documents to model.
#' @param model_name A character vector of SentenceTransformer model names.
#'   Embeddings are computed once per model. Default is
#'   \code{"all-MiniLM-L6-v2"}.
#' @param n_neighbors An integer vector supplied to UMAP, controlling the
#'   local neighborhood size. Larger values emphasize global structure;
#'   smaller values emphasize local structure.
#' @param n_components An integer vector giving the dimensionality of the
#'   UMAP projection.
#' @param metric Distance metric used by UMAP. Default is \code{"cosine"}.
#' @param minPts An integer vector specifying the minimum cluster size for
#'   HDBSCAN. Larger values produce fewer, more robust clusters; smaller
#'   values allow more fine-grained clusters.
#'
#' @return A data frame where each row corresponds to a parameter combination
#'   and columns include:
#'   \itemize{
#'     \item \code{model_name}
#'     \item \code{n_neighbors}
#'     \item \code{n_components}
#'     \item \code{minPts}
#'     \item \code{n_topics}
#'     \item \code{n_outliers}
#'     \item \code{perc_outliers}
#'   }
#'
#' @examples
#' \dontrun{
#' texts <- c("Text about cats", "Another cat text",
#'            "A document about dogs", "More dog content")
#'
#' grid_results <- identify_topics_grid(
#'   texts       = texts,
#'   model_name  = "all-MiniLM-L6-v2",
#'   n_neighbors = c(10, 15, 30),
#'   n_components = 10,
#'   minPts      = c(5, 10)
#' )
#'
#' print(grid_results)
#' }
#'
#' @export
identify_topics_grid <- function(
    texts,
    model_name           = "all-MiniLM-L6-v2",   # can be vector
    n_neighbors          = 15,                   # can be vector
    n_components         = 10,                   # can be vector
    metric               = "cosine",
    minPts               = 10
) {

  # ---- 0. Coerce to vectors (in case user passes scalars) ----
  model_name  <- as.character(model_name)
  n_neighbors <- as.integer(n_neighbors)
  n_components <- as.integer(n_components)
  minPts      <- as.integer(minPts)

  # ---- 1. Import Python modules ----
  sentence_transformers <- reticulate::import("sentence_transformers")
  np                    <- reticulate::import("numpy")

  results <- list()

  # ---- 2. Loop over models: embeddings computed ONCE per model ----
  for (m in model_name) {

    message("Loading model: ", m, " ...")
    model <- sentence_transformers$SentenceTransformer(m)

    message("Encoding texts into embeddings...")
    emb    <- model$encode(texts, show_progress_bar = TRUE)
    emb_np <- np$array(emb)
    emb_r  <- reticulate::py_to_r(emb_np)

    # ---- 3. For this model, loop over UMAP settings ----
    for (nn in n_neighbors) {
      for (nc in n_components) {

        message(sprintf(
          "Running UMAP (model = %s, n_neighbors = %d, n_components = %d)...",
          m, nn, nc
        ))

        set.seed(42)
        embeddings_umap <- uwot::umap(
          emb_r,
          n_neighbors  = nn,
          n_components = nc,
          metric       = metric
        )

        # ---- 4. For this UMAP, loop over HDBSCAN minPts ----
        for (mp in minPts) {

          message(sprintf(
            "Running HDBSCAN (model = %s, n_neighbors = %d, n_components = %d, minPts = %d)...",
            m, nn, nc, mp
          ))

          clust <- dbscan::hdbscan(
            embeddings_umap,
            minPts      = mp
          )

          clusters <- clust$cluster

          n_docs <- length(clusters)

          n_outliers    <- sum(clusters == 0L)
          perc_outliers <- n_outliers / n_docs
          n_topics      <- length(setdiff(unique(clusters), 0L))

          # ---- 5. Store summary for this combination ----
          results[[length(results) + 1L]] <- data.frame(
            model_name           = m,
            n_neighbors          = nn,
            n_components         = nc,
            minPts               = mp,
            n_topics             = n_topics,
            n_outliers           = n_outliers,
            perc_outliers        = perc_outliers
          )
        }
      }
    }
  }

  # ---- 6. Bind into one data.frame ----
  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out
}
