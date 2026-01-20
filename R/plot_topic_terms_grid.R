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
#' @param topic_labels A dataframe with two columns ("cluster" and "label") that
#'   will put a label on the topics.
#'
#' @return A ggplot object with facetted barplots.
plot_topic_terms_grid <- function(topic_terms,
                                  n_topics     = 6,
                                  n_terms      = 10,
                                  metric       = c("tfidf", "n"),
                                  topics       = NULL,
                                  topic_labels = NULL
) {

  metric <- match.arg(metric)

  # Basic checks -------------------------------------------------------------
  required_cols <- c("topic", "term", metric)
  missing_cols  <- setdiff(required_cols, names(topic_terms))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Optional labels checks ---------------------------------------------------
  if (!is.null(topic_labels)) {
    req_lab <- c("cluster", "label")
    miss_lab <- setdiff(req_lab, names(topic_labels))
    if (length(miss_lab) > 0) {
      stop("`topic_labels` must contain columns: ", paste(req_lab, collapse = ", "))
    }
    topic_labels <- topic_labels |>
      dplyr::select(.data$cluster, .data$label) |>
      dplyr::mutate(
        cluster = as.character(.data$cluster),
        label   = as.character(.data$label)
      )

    # If duplicate clusters exist, keep first (avoid ambiguous mapping)
    if (any(duplicated(topic_labels$cluster))) {
      topic_labels <- topic_labels[!duplicated(topic_labels$cluster), , drop = FALSE]
      warning("Duplicate `cluster` values found in `topic_labels`; keeping the first occurrence per cluster.")
    }
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

  # Build facet label mapping (keep facets by topic, display labels) ----------
  topics_use_chr <- as.character(topics_use)
  label_map <- stats::setNames(paste0("Topic ", topics_use_chr), topics_use_chr)  # default fallback

  if (!is.null(topic_labels)) {
    m <- topic_labels |>
      dplyr::filter(.data$cluster %in% topics_use_chr)
    if (nrow(m) > 0) {
      label_map[m$cluster] <- m$label
    }
  }

  # Subset + take top n_terms for each topic --------------------------------
  topic_terms_top <- topic_terms |>
    dplyr::filter(.data$topic %in% topics_use) |>
    dplyr::group_by(.data$topic) |>
    dplyr::arrange(dplyr::desc(.data[[metric]]), .by_group = TRUE) |>
    dplyr::slice_head(n = n_terms) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      topic      = factor(.data$topic, levels = topics_use),
      term_topic = paste(.data$term, .data$topic, sep = "___"),
      term_topic = stats::reorder(.data$term_topic, .data[[metric]])
    )

  # Build the plot -----------------------------------------------------------
  n_topics_final <- length(topics_use)
  ncol_grid <- ceiling(sqrt(n_topics_final))

  ggplot2::ggplot(
    topic_terms_top,
    ggplot2::aes(x = term_topic, y = .data[[metric]], fill = .data$topic)
  ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(
      ~ topic,
      ncol     = ncol_grid,
      scales   = "free_y",
      labeller = ggplot2::labeller(topic = ggplot2::as_labeller(label_map))
    ) +
    ggplot2::scale_x_discrete(labels = function(x) sub("___.*$", "", x)) +
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
}
