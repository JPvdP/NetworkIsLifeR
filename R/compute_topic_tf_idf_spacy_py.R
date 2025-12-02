#' Compute topic-level term scores using spaCy (Python)
#'
#' This function takes a data frame with documents, their topic (cluster)
#' assignments, and raw text, and computes term statistics per topic using a
#' spaCy (Python) model. It tokenises the text, keeps only selected part-of-speech
#' tags (e.g. nouns and proper nouns), optionally builds n-grams, filters
#' short and infrequent terms, and computes term scores (TF or TF–IDF) per
#' topic. The output is a tidy data frame with the top terms for each topic.
#'
#' @param data A data frame (or tibble) containing at least the document
#'   identifier, topic/cluster label, and text columns.
#' @param doc_col A string giving the name of the column in \code{data}
#'   that contains unique document identifiers. Default is \code{"doc_id"}.
#' @param topic_col A string giving the name of the column in \code{data}
#'   that contains the topic or cluster assignment for each document.
#'   Default is \code{"cluster"}.
#' @param text_col A string giving the name of the column in \code{data}
#'   that contains the raw text to be analysed. Default is \code{"text"}.
#' @param spacy_model A string with the name of the spaCy model to use,
#'   e.g. \code{"en_core_web_sm"}. The model must be installed and available
#'   in the active Python environment. Default is \code{"en_core_web_sm"}.
#' @param pos_keep A character vector of coarse-grained POS tags (as used
#'   by spaCy, e.g. \code{"NOUN"}, \code{"PROPN"}) that should be retained
#'   when computing term statistics. Tokens with other POS tags are discarded.
#'   Default is \code{c("NOUN", "PROPN")}.
#' @param min_char Integer; minimum number of characters a token must have
#'   to be kept. Shorter tokens are discarded. Default is \code{3L}.
#' @param min_term_freq Integer; minimum term frequency (across all
#'   documents/topics) required for a term to be kept. Terms that occur fewer
#'   times than this threshold are removed. Default is \code{2L}.
#' @param top_n Integer; number of top-ranking terms to return per topic
#'   after scoring (e.g. by TF–IDF). Default is \code{10L}.
#' @param stopwords Optional character vector of stopwords to remove
#'   before computing term statistics. If \code{NULL} (default), no
#'   additional stopwords are removed apart from those filtered by
#'   POS/length/frequency.
#' @param use_tfidf Logical; if \code{TRUE} (default), compute TF–IDF
#'   scores per topic. If \code{FALSE}, only term frequencies are computed
#'   and returned.
#' @param exclude_topics Optional vector of topic labels (values found in
#'   \code{topic_col}) that should be excluded from the computation. If
#'   \code{NULL} (default), all topics are included.
#' @param min_ngram Integer; minimum n-gram size to construct from the
#'   token sequence. Default is \code{1L} (unigrams).
#' @param max_ngram Integer; maximum n-gram size to construct from the
#'   token sequence. Must be greater than or equal to \code{min_ngram}.
#'   Default is \code{1L} (no n-grams beyond unigrams).
#' @param ngram_sep A single-character string used to join tokens when
#'   forming n-grams. Default is a space (\code{" "}).
#'
#' @details
#' Internally, this function relies on a Python spaCy pipeline (accessed
#' via \pkg{reticulate}) to perform tokenisation and POS-tagging. Make sure
#' that a suitable Python environment is available, spaCy is installed in
#' that environment, and the specified \code{spacy_model} has been downloaded.
#'
#' The function aggregates terms at the topic level, filters them according
#' to the length and frequency thresholds, optionally constructs n-grams, and
#' then computes per-topic term scores. When \code{use_tfidf = TRUE}, a
#' TF–IDF-like weighting is used to highlight terms that are frequent within
#' a topic but relatively rare across other topics.
#'
#' @return A tibble or data frame with one row per (topic, term) combination
#'   and at least the following columns:
#'   \itemize{
#'     \item \code{topic} (or the values from \code{topic_col})
#'     \item \code{term} (unigrams or n-grams)
#'     \item term frequency and, if requested, TF–IDF score
#'     \item a rank or ordering within each topic (if implemented)
#'   }
#'   The exact column names may depend on the implementation.
#'
#' @examples
#' \dontrun{
#' # Minimal example
#' library(tibble)
#'
#' toy_data <- tibble::tibble(
#'   doc_id  = c("d1", "d2", "d3"),
#'   cluster = c(1, 1, 2),
#'   text    = c(
#'     "Solar energy systems and photovoltaic panels",
#'     "Photovoltaic modules for sustainable energy",
#'     "Wind turbines and offshore energy production"
#'   )
#' )
#'
#' topic_terms <- compute_topic_tf_idf_spacy_py(
#'   data       = toy_data,
#'   doc_col    = "doc_id",
#'   topic_col  = "cluster",
#'   text_col   = "text",
#'   spacy_model = "en_core_web_sm",
#'   pos_keep    = c("NOUN", "PROPN"),
#'   min_ngram   = 1L,
#'   max_ngram   = 2L,
#'   top_n       = 5L
#' )
#' }
#' @export
compute_topic_tf_idf_spacy_py <- function(data,
                                           doc_col        = "doc_id",
                                           topic_col      = "cluster",
                                           text_col       = "text",
                                           spacy_model    = "en_core_web_sm",
                                           pos_keep       = c("NOUN", "PROPN"),
                                           min_char       = 3L,
                                           min_term_freq  = 2L,
                                           top_n          = 10L,
                                           stopwords      = NULL,
                                           use_tfidf      = TRUE,
                                           exclude_topics = NULL,
                                           min_ngram      = 1L,
                                           max_ngram      = 1L,
                                           ngram_sep      = " ") {

  # --- Dependencies ----------------------------------------------------
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required but not installed.")
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required but not installed.")
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("The 'rlang' package is required but not installed.")
  }

  # Ensure Python is initialized in the correct env
  if (!reticulate::py_available(initialize = TRUE)) {
    stop(
      "Python is not available via reticulate. ",
      "Did you forget to call start_bertopicr() or use_virtualenv() first?"
    )
  }

  # --- Standardise input columns ---------------------------------------
  doc_sym   <- rlang::sym(doc_col)
  topic_sym <- rlang::sym(topic_col)
  text_sym  <- rlang::sym(text_col)

  df <- data %>%
    dplyr::select(
      doc_id = !!doc_sym,
      topic  = !!topic_sym,
      text   = !!text_sym
    )

  if (!is.null(exclude_topics)) {
    df <- df %>%
      dplyr::filter(!topic %in% exclude_topics)
  }

  # --- Build one pseudo-document per topic -----------------------------
  # This is the key change: term extraction is done on per-topic corpora,
  # i.e. on the subset of documents belonging to each topic.
  df_topic <- df %>%
    dplyr::group_by(topic) %>%
    dplyr::summarise(
      text = paste(text, collapse = "\n"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(doc_id = as.character(topic)) %>%
    dplyr::select(doc_id, topic, text)

  # --- Define the Python helper function (always) ----------------------
  py_code <- "
import spacy
import spacy.cli

_nlp_cache = {}

def _get_nlp(model):
    if model not in _nlp_cache:
        try:
            _nlp_cache[model] = spacy.load(model)
        except OSError:
            # Model not installed: download and try again (first call only)
            spacy.cli.download(model)
            _nlp_cache[model] = spacy.load(model)
    return _nlp_cache[model]

def parse_for_topics(texts, doc_ids, topics, model,
                     pos_keep=('NOUN','PROPN'),
                     min_char=3,
                     min_ngram=1,
                     max_ngram=1,
                     sep=' '):

    nlp = _get_nlp(model)
    results = []

    for text, doc_id, topic in zip(texts, doc_ids, topics):
        if text is None:
            continue
        doc = nlp(text)

        # Collect lemmas of tokens we keep
        lemmas = []
        for token in doc:
            if token.pos_ not in pos_keep:
                continue
            lemma = token.lemma_.lower()
            if len(lemma) < min_char:
                continue
            if lemma.isnumeric():
                continue
            lemmas.append(lemma)

        if not lemmas:
            continue

        # Generate n-grams on the lemma sequence
        L = len(lemmas)
        for n in range(min_ngram, max_ngram + 1):
            if n <= 0:
                continue
            if L < n:
                continue
            for i in range(L - n + 1):
                term = sep.join(lemmas[i:i+n])
                results.append({
                    'doc_id': doc_id,
                    'topic': topic,
                    'term': term,
                    'ngram_n': n
                })

    return results
"
  reticulate::py_run_string(py_code, convert = FALSE)

  # --- Call Python to parse topic-level texts --------------------------
  texts   <- as.character(df_topic$text)
  doc_ids <- as.character(df_topic$doc_id)
  topics  <- df_topic$topic

  res <- reticulate::py$parse_for_topics(
    texts,
    doc_ids,
    topics,
    spacy_model,
    pos_keep,
    as.integer(min_char),
    as.integer(min_ngram),
    as.integer(max_ngram),
    ngram_sep
  )

  if (length(res) == 0) {
    warning("spaCy returned no tokens; check model, pos_keep, min_char, and n-gram settings.")
    return(tibble::tibble(
      topic = character(),
      term  = character(),
      n     = integer(),
      n_topics_with_term = integer(),
      idf   = numeric(),
      tfidf = numeric(),
      rank  = integer()
    ))
  }

  tokens <- dplyr::bind_rows(res)  # list of dicts -> tibble

  # --- Apply stopwords and other filters on the R side -----------------
  tokens <- tokens %>%
    dplyr::filter(!is.na(topic), !is.na(term), term != "")

  if (!is.null(stopwords)) {
    sw <- tolower(stopwords)
    tokens <- tokens %>%
      dplyr::filter(!term %in% sw)
  }

  if (nrow(tokens) == 0) {
    warning("No tokens left after filtering; check stopwords / filters.")
    return(tibble::tibble(
      topic = character(),
      term  = character(),
      n     = integer(),
      n_topics_with_term = integer(),
      idf   = numeric(),
      tfidf = numeric(),
      rank  = integer()
    ))
  }

  # --- Term counts per topic (full) ------------------------------------
  term_counts_full <- tokens %>%
    dplyr::count(topic, term, name = "n")

  if (nrow(term_counts_full) == 0) {
    warning("No terms available to count; something went wrong after tokenisation.")
    return(tibble::tibble(
      topic = character(),
      term  = character(),
      n     = integer(),
      n_topics_with_term = integer(),
      idf   = numeric(),
      tfidf = numeric(),
      rank  = integer()
    ))
  }

  # --- Compute TF–IDF across topics ------------------------------------
  n_topics <- dplyr::n_distinct(term_counts_full$topic)

  df_term_topics <- term_counts_full %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(
      n_topics_with_term = dplyr::n(),
      .groups = "drop"
    )

  term_scores_full <- term_counts_full %>%
    dplyr::left_join(df_term_topics, by = "term")

  if (use_tfidf) {
    term_scores_full <- term_scores_full %>%
      dplyr::mutate(
        idf   = log((1 + n_topics) / (1 + n_topics_with_term)) + 1,
        tfidf = n * idf
      )
  } else {
    term_scores_full <- term_scores_full %>%
      dplyr::mutate(
        idf   = NA_real_,
        tfidf = as.numeric(n)
      )
  }

  # --- Select top N terms per topic, with fallback ---------------------
  # Primary: enforce min_term_freq within each topic
  top_terms_primary <- term_scores_full %>%
    dplyr::group_by(topic) %>%
    dplyr::filter(n >= min_term_freq) %>%
    dplyr::arrange(dplyr::desc(tfidf), dplyr::desc(n), .by_group = TRUE) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::ungroup()

  # Identify topics that would be empty after min_term_freq
  topics_all      <- unique(term_scores_full$topic)
  topics_with_any <- unique(top_terms_primary$topic)
  topics_missing  <- setdiff(topics_all, topics_with_any)

  # Fallback: for those topics, ignore min_term_freq and just take the best terms
  fallback <- term_scores_full %>%
    dplyr::filter(topic %in% topics_missing) %>%
    dplyr::group_by(topic) %>%
    dplyr::arrange(dplyr::desc(tfidf), dplyr::desc(n), .by_group = TRUE) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::ungroup()

  if (length(topics_missing) > 0L) {
    message(
      "Some topics had no terms with n >= min_term_freq. ",
      "For these topics, the threshold was relaxed and the best terms were used instead: ",
      paste(topics_missing, collapse = ", ")
    )
  }

  top_terms <- dplyr::bind_rows(top_terms_primary, fallback) %>%
    dplyr::group_by(topic) %>%
    dplyr::arrange(dplyr::desc(tfidf), dplyr::desc(n), .by_group = TRUE) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(topic, rank)

  top_terms
}
