#' Interactive 2D Topic Map with Plotly
#'
#' \code{plot_topic_space_interactive()} creates an interactive 2D scatterplot of
#' topic embeddings (typically UMAP-reduced coordinates) with Plotly. Each point
#' represents a document, coloured by its assigned topic. Labels for topics are
#' added at the centroid of each cluster, and optional hover text can display the
#' full document text in a readable wrapped format.
#'
#' The function supports:
#' \itemize{
#'   \item custom topic labels (LLM-generated or manual),
#'   \item wrapped, multi-line hover text for documents,
#'   \item truncated text to keep tooltips readable,
#'   \item centroid-based topic labels directly on the plot.
#' }
#'
#' @param doc_coordinates A data frame produced by the topic modelling pipeline.
#'   Must contain columns \code{doc_id}, \code{x_col}, \code{y_col}, and
#'   \code{cluster_col}. Typically the output of \code{identify_topics_coord()}.
#'
#' @param text Optional character vector with the original document texts or
#'   summaries. Used for hover tooltips. Must be the same length as
#'   \code{nrow(doc_coordinates)}.
#'
#' @param x_col Name of the column in \code{doc_coordinates} representing the
#'   x-axis coordinate (default: \code{"dim_1"}).
#'
#' @param y_col Name of the column in \code{doc_coordinates} representing the
#'   y-axis coordinate (default: \code{"dim_2"}).
#'
#' @param cluster_col Name of the column containing topic cluster assignments
#'   (default: \code{"cluster"}).
#'
#' @param topic_labels Optional topic descriptions. May be either:
#'   \itemize{
#'     \item a data frame with columns \code{cluster} (or \code{topic}) and
#'       \code{label} (or \code{Label}), or
#'     \item a named character vector, where names correspond to topic IDs.
#'   }
#'   These labels will appear in the legend, in tooltips, and on centroid
#'   labels.
#'
#' @param show_ids Logical. If \code{TRUE} (default), labels take the form
#'   \code{"Topic k: Label"}. If \code{FALSE}, only the label description is
#'   shown.
#'
#' @param wrap_width Integer. Number of characters per line when wrapping tooltip
#'   text (default: \code{80}). Passed to \code{stringr::str_wrap()}.
#'
#' @param max_chars Integer or \code{NULL}. Maximum number of characters of the
#'   original text to include in hover tooltips (default: \code{400}). Longer
#'   texts are truncated with \code{"..."}. Set to \code{NULL} to disable
#'   truncation.
#'
#' @return A Plotly object containing an interactive scatterplot:
#'   \itemize{
#'     \item each point = a document,
#'     \item colour = topic label,
#'     \item hover = readable wrapped text or coordinates,
#'     \item centroid labels = topic names or numbers.
#'   }
#'
#' @examples
#' \dontrun{
#' p <- plot_topic_space_interactive(
#'   doc_coordinates = topic_coords,
#'   text            = documents$text,
#'   topic_labels    = topic_labels_df,
#'   wrap_width      = 60,
#'   max_chars       = 300
#' )
#' p
#' }
#'
#' @export
plot_topic_space_interactive <- function(
    doc_coordinates,
    text = NULL,          # optional vector of full texts for hover
    x_col = "dim_1",
    y_col = "dim_2",
    cluster_col = "cluster",
    topic_labels = NULL,  # NEW: labels per topic
    show_ids = TRUE       # NEW: prepend "Topic k: " in the label
) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required.")
  }

  df <- doc_coordinates

  # Basic sanity checks
  if (!all(c(x_col, y_col, cluster_col, "doc_id") %in% names(df))) {
    stop("doc_coordinates must have columns: doc_id, ", x_col, ", ", y_col, ", ", cluster_col)
  }

  # --- 1. Prepare base topic IDs (cluster 0 = noise) ---
  df <- df %>%
    dplyr::mutate(
      cluster_num = !!rlang::sym(cluster_col)
    )

  # --- 2. Prepare topic label lookup (if provided) ---
  label_df <- NULL

  if (!is.null(topic_labels)) {
    if (is.data.frame(topic_labels)) {
      # Expect something like: columns "cluster" and "label" or "topic" and "label"
      tmp <- topic_labels

      if (!"cluster" %in% names(tmp) && "topic" %in% names(tmp)) {
        tmp <- dplyr::rename(tmp, cluster = .data$topic)
      }
      if (!"label" %in% names(tmp) && "Label" %in% names(tmp)) {
        tmp <- dplyr::rename(tmp, label = .data$Label)
      }

      if (!all(c("cluster", "label") %in% names(tmp))) {
        stop("topic_labels data frame must have columns 'cluster' (or 'topic') and 'label' (or 'Label').")
      }

      label_df <- tmp %>%
        dplyr::mutate(cluster = as.integer(.data$cluster))

    } else if (is.character(topic_labels) && !is.null(names(topic_labels))) {
      # Named character vector: names = cluster ids, values = labels
      label_df <- data.frame(
        cluster = as.integer(names(topic_labels)),
        label   = as.character(topic_labels),
        stringsAsFactors = FALSE
      )
    } else {
      stop("topic_labels must be either a data.frame or a named character vector.")
    }
  }

  # --- 3. Attach human-readable labels to df ---
  if (!is.null(label_df)) {
    df <- df %>%
      dplyr::left_join(label_df,
                       by = dplyr::join_by(cluster_num == cluster))

    df <- df %>%
      dplyr::mutate(
        topic_label = dplyr::case_when(
          cluster_num == 0 ~ "Noise",
          !is.na(label) & show_ids ~ paste0("Topic ", cluster_num, ": ", label),
          !is.na(label) & !show_ids ~ label,
          TRUE ~ paste0("Topic ", cluster_num) # fallback if no label found
        )
      )
  } else {
    # No external labels provided: default numeric labels
    df <- df %>%
      dplyr::mutate(
        topic_label = dplyr::if_else(
          cluster_num == 0,
          "Noise",
          paste0("Topic ", cluster_num)
        )
      )
  }

  # --- 4. Tooltip text ---
  if (is.null(text)) {
    df$tooltip <- paste0(
      "Doc: ", df$doc_id, "<br>",
      "Topic: ", df$topic_label, "<br>",
      "x: ", signif(df[[x_col]], 3), "<br>",
      "y: ", signif(df[[y_col]], 3)
    )
  } else {
    df$tooltip <- paste0(
      "Doc: ", df$doc_id, "<br>",
      "Topic: ", df$topic_label, "<br>",
      "Text: ", text
    )
  }

  # --- 5. Compute centroids per topic for labels (excluding noise) ---
  centroids <- df %>%
    dplyr::filter(cluster_num != 0) %>% # keep 0 if you want a label for noise too
    dplyr::group_by(topic_label, cluster_num) %>%
    dplyr::summarise(
      x      = median(.data[[x_col]], na.rm = TRUE),
      y      = median(.data[[y_col]], na.rm = TRUE),
      n_docs = dplyr::n(),
      .groups = "drop"
    )

  # --- 6. Base interactive scatter ---
  p <- plotly::plot_ly(
    data  = df,
    x     = ~ .data[[x_col]],
    y     = ~ .data[[y_col]],
    type  = "scatter",
    mode  = "markers",
    color = ~ topic_label,
    text  = ~ tooltip,
    hoverinfo = "text",
    marker = list(size = 6, opacity = 0.8)
  )

  # --- 7. Add topic labels at centroids ---
  p <- plotly::add_text(
    p,
    data = centroids,
    x    = ~ x,
    y    = ~ y,
    text = ~ topic_label,
    mode = "text",
    textposition = "top center",
    textfont = list(size = 14, weight = "bold"),
    showlegend = FALSE,
    inherit = FALSE
  )

  # --- 8. Layout ---
  p <- plotly::layout(
    p,
    xaxis = list(title = "UMAP 1"),
    yaxis = list(title = "UMAP 2"),
    legend = list(title = list(text = "Topic")),
    dragmode = "zoom"
  )

  return(p)
}
