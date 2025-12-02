#' Plot top terms per topic as small barplots
#'
#' @param topic_terms A data.frame/tibble with columns:
#'   - topic
#'   - term
#'   - tfidf (or another metric you choose via `metric`)
#' @param n_topics How many topics to visualise (default: 6).
#'   If there are fewer topics than this, all topics are shown.
#' @param n_terms How many top terms per topic (default: 10).
#' @param metric Which column to use for bar height (default: "tfidf").
#'   You can set this to "n" if you want raw counts instead.
#' @param topics Optional vector of topic IDs to plot explicitly.
#'   If provided, this overrides `n_topics`.
#'
#' @return A ggplot object with facetted barplots.
plot_topic_terms_grid <- function(topic_terms,
                                  n_topics = 6,
                                  n_terms  = 10,
                                  metric   = c("tfidf", "n"),
                                  topics   = NULL) {

  metric <- match.arg(metric)

  # Basic checks -------------------------------------------------------------
  required_cols <- c("topic", "term", metric)
  missing_cols  <- setdiff(required_cols, names(topic_terms))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Decide which topics to plot ---------------------------------------------
  all_topics <- unique(topic_terms$topic)

  if (!is.null(topics)) {
    topics_use <- intersect(topics, all_topics)
    if (length(topics_use) == 0L) {
      stop("None of the requested topics are present in `topic_terms`.")
    }
  } else {
    topics_use <- head(all_topics, n_topics)
  }

  # Subset + take top n_terms for each topic --------------------------------
  # We create a combined factor "term___topic" so we can reorder within each
  # topic for nice descending bars, then strip the suffix from axis labels.
  topic_terms_top <- topic_terms |>
    dplyr::filter(.data$topic %in% topics_use) |>
    dplyr::group_by(.data$topic) |>
    dplyr::arrange(dplyr::desc(.data[[metric]]), .by_group = TRUE) |>
    dplyr::slice_head(n = n_terms) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      topic = factor(.data$topic, levels = topics_use),
      term_topic = paste(.data$term, .data$topic, sep = "___"),
      term_topic = stats::reorder(.data$term_topic, .data[[metric]])
    )

  # Build the plot ----------------------------------------------------------
  n_topics_final <- length(topics_use)
  ncol_grid <- ceiling(sqrt(n_topics_final))

  p <- ggplot2::ggplot(
    topic_terms_top,
    ggplot2::aes(x = term_topic, y = .data[[metric]], fill = .data$topic)
  ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(
      ~ topic,
      ncol   = ncol_grid,
      scales = "free_y"
    ) +
    # Clean up x labels: remove the "___topic" suffix
    ggplot2::scale_x_discrete(
      labels = function(x) sub("___.*$", "", x)
    ) +
    ggplot2::labs(
      x     = NULL,
      y     = metric,
      title = "Top terms per topic"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      strip.text  = ggplot2::element_text(face = "bold"),
      axis.text.y = ggplot2::element_text(size = 8),
      plot.title  = ggplot2::element_text(hjust = 0, face = "bold")
    )

  p
}
