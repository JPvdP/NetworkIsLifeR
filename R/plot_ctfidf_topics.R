#' plot_ctfidf_topics
#'
#' This function visualizes class-based TF-IDF (c-TF-IDF) topic representations
#' by creating bar charts of the top words per topic. It can display a single
#' topic, several topics, or all topics contained in the c-TF-IDF table.
#'
#' @param ctfidf_df A dataframe produced by \code{compute_ctfidf()}, containing
#'   the columns \code{topic}, \code{word}, and \code{score}.
#' @param topics A vector of topic IDs to plot. Defaults to \code{NULL}, which
#'   plots all available topics.
#' @param n_words Number of top words (ranked by c-TF-IDF score) to show per
#'   topic. Default is 10.
#' @param palette A ColorBrewer palette name used to color the bars.
#'   Default is \code{"Set2"}.
#' @param facet Logical; if \code{TRUE}, produces a faceted plot with one panel
#'   per topic. If \code{FALSE}, topics are plotted together. Default is FALSE.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' # Assuming `ctfidf_df` is output from compute_ctfidf()
#' plot_ctfidf_topics(ctfidf_df, topics = 1, n_words = 10)
#' plot_ctfidf_topics(ctfidf_df, facet = TRUE)
#' }
#' @export
plot_ctfidf_topics <- function(
    ctfidf_df,
    topics = NULL,
    n_words = 10,
    palette = "Set2",
    facet = FALSE
) {

  # If no topics specified, plot all
  if (is.null(topics)) {
    topics <- sort(unique(ctfidf_df$topic))
  }

  # Select and reorder top words
  df_plot <- ctfidf_df %>%
    dplyr::filter(topic %in% topics) %>%
    dplyr::group_by(topic) %>%
    dplyr::slice_max(order_by = score, n = n_words) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(word = reorder_within(word, score, topic))

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(
    x = word, y = score, fill = factor(topic)
  )) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_brewer(palette = palette) +
    ggplot2::coord_flip() +
    scale_x_reordered() +
    ggplot2::labs(
      title = "c-TF-IDF Topic Representation",
      x = "Word",
      y = "c-TF-IDF Score"
    ) +
    ggplot2::theme_minimal(base_size = 13)

  if (facet) {
    p <- p + ggplot2::facet_wrap(~ topic, scales = "free_y")
  }

  return(p)
}
