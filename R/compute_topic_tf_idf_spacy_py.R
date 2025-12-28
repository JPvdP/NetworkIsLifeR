#' Compute topic-level salient terms using spaCy (Python) with per-document parsing
#'
#' This function takes a data frame with documents, their topic (cluster)
#' assignments, and raw text, and computes term statistics per topic using a
#' spaCy (Python) language model. Unlike approaches that concatenate all texts
#' within each topic into a single pseudo-document, this function processes text
#' per document (streamed via \code{nlp.pipe()}) and aggregates term counts at the
#' topic level. This design is more memory-efficient, avoids spaCy's maximum
#' text-length constraint for large topics, and prevents n-grams spanning
#' document boundaries.
#'
#' Tokenisation, POS-tagging, and lemmatisation are performed in Python via
#' \pkg{reticulate}. Tokens are filtered to retain only selected spaCy POS tags
#' (e.g. nouns and proper nouns), short tokens are removed, and optional n-grams
#' are constructed over the retained lemma sequence within each document. The
#' function then aggregates term frequencies per topic and computes either raw
#' term frequency (TF) or a TF–IDF-like score across topics to highlight terms
#' that are frequent within a topic but relatively rare in other topics.
#'
#' For efficiency, term counting is performed in Python and only aggregated
#' (topic, term, frequency) results are returned to R. The output is a tidy data
#' frame containing the top-ranking terms per topic, with a fallback mechanism
#' that relaxes \code{min_term_freq} for topics that would otherwise return no
#' terms.
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
#' @param min_char Integer; minimum number of characters a (lemmatised) token
#'   must have to be kept. Shorter tokens are discarded. Default is \code{3L}.
#' @param min_term_freq Integer; minimum within-topic term frequency required for
#'   a term to be eligible for the primary top-\code{n} selection. If a topic has
#'   no terms meeting this threshold, a fallback step relaxes this constraint and
#'   returns the best-scoring terms anyway. Default is \code{2L}.
#' @param top_n Integer; number of top-ranking terms to return per topic after
#'   scoring (by TF–IDF or TF). Default is \code{10L}.
#' @param stopwords Optional character vector of stopwords to remove before
#'   computing term statistics. Stopword matching is case-insensitive and applied
#'   to the final term string (i.e. exact-match on unigrams or n-grams). If
#'   \code{NULL} (default), no additional stopwords are removed.
#' @param use_tfidf Logical; if \code{TRUE} (default), compute TF–IDF-like scores
#'   per topic. If \code{FALSE}, only term frequencies are used for ranking.
#' @param exclude_topics Optional vector of topic labels (values found in
#'   \code{topic_col}) that should be excluded from computation. If \code{NULL}
#'   (default), all topics are included.
#' @param min_ngram Integer; minimum n-gram size to construct from the retained
#'   lemma sequence within each document. Default is \code{1L} (unigrams).
#' @param max_ngram Integer; maximum n-gram size to construct from the retained
#'   lemma sequence within each document. Must be greater than or equal to
#'   \code{min_ngram}. Default is \code{1L} (no n-grams beyond unigrams).
#' @param ngram_sep A single-character string used to join tokens when forming
#'   n-grams. Default is a space (\code{" "}).
#'
#' @details
#' Internally, this function relies on a Python spaCy pipeline (accessed via
#' \pkg{reticulate}) to perform tokenisation, POS-tagging, and lemmatisation. For
#' performance and memory efficiency, documents are processed using
#' \code{nlp.pipe()} and term counts are aggregated in Python before being
#' returned to R.
#'
#' The TF–IDF-like weighting is computed across topics. For each term, the
#' document frequency is defined as the number of topics in which the term occurs
#' at least once. The inverse document frequency is:
#' \deqn{idf = log((1 + T)/(1 + df)) + 1}
#' where \eqn{T} is the number of topics and \eqn{df} is the number of topics
#' containing the term. Term scores are then \eqn{tfidf = tf * idf}, where
#' \eqn{tf} is the within-topic term frequency.
#'
#' @return A tibble with one row per (topic, term) combination in the top-\code{n}
#'   list for each topic, with at least the following columns:
#'   \itemize{
#'     \item \code{topic}: topic label (values from \code{topic_col})
#'     \item \code{term}: unigram or n-gram term string (lemmatised, lowercased)
#'     \item \code{n}: within-topic term frequency
#'     \item \code{n_topics_with_term}: number of topics in which the term appears
#'     \item \code{idf}: inverse document frequency across topics (if enabled)
#'     \item \code{tfidf}: TF–IDF-like score used for ranking
#'     \item \code{rank}: rank of the term within topic (1 = highest)
#'   }
#'
#' @examples
#' \dontrun{
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
#' # Unigrams + bigrams, TF–IDF
#' topic_terms <- compute_topic_tf_idf_spacy_py(
#'   data        = toy_data,
#'   doc_col     = "doc_id",
#'   topic_col   = "cluster",
#'   text_col    = "text",
#'   spacy_model = "en_core_web_sm",
#'   pos_keep    = c("NOUN", "PROPN"),
#'   min_ngram   = 1L,
#'   max_ngram   = 2L,
#'   top_n       = 5L
#' )
#'
#' # TF only (no IDF weighting)
#' topic_terms_tf <- compute_topic_tf_idf_spacy_py(
#'   data       = toy_data,
#'   use_tfidf  = FALSE,
#'   top_n      = 5L
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
  if (!requireNamespace("reticulate", quietly = TRUE)) stop("The 'reticulate' package is required but not installed.")
  if (!requireNamespace("dplyr", quietly = TRUE))      stop("The 'dplyr' package is required but not installed.")
  if (!requireNamespace("tibble", quietly = TRUE))     stop("The 'tibble' package is required but not installed.")
  if (!requireNamespace("rlang", quietly = TRUE))      stop("The 'rlang' package is required but not installed.")

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
    df <- df %>% dplyr::filter(!topic %in% exclude_topics)
  }

  # Drop missing/empty texts early
  df <- df %>%
    dplyr::filter(!is.na(text), text != "")

  if (nrow(df) == 0) {
    warning("No documents to process after filtering.")
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

  # --- Define Python helper (count in Python; use nlp.pipe) -------------
  py_code <- "
import spacy
import spacy.cli
from collections import Counter

_nlp_cache = {}

def _get_nlp(model, max_length_needed=None):
    if model not in _nlp_cache:
        try:
            # We only need POS + lemma; disable heavy components for speed/memory
            _nlp_cache[model] = spacy.load(model, exclude=['parser', 'ner', 'textcat'])
        except OSError:
            spacy.cli.download(model)
            _nlp_cache[model] = spacy.load(model, exclude=['parser', 'ner', 'textcat'])

    nlp = _nlp_cache[model]

    # Safety: raise max_length if needed (should rarely be necessary per-document)
    if max_length_needed is not None and max_length_needed > nlp.max_length:
        nlp.max_length = int(max_length_needed)

    return nlp

def parse_and_count_terms(texts, topics, model,
                          pos_keep=('NOUN','PROPN'),
                          min_char=3,
                          min_ngram=1,
                          max_ngram=1,
                          sep=' ',
                          batch_size=128):

    # Determine max doc length to avoid E088 (per-doc should usually be below default)
    max_len = 0
    for t in texts:
        if t is not None:
            lt = len(t)
            if lt > max_len:
                max_len = lt

    nlp = _get_nlp(model, max_length_needed=max_len + 1000)

    # Count terms per topic efficiently
    counts = Counter()

    # Stream docs
    for i, doc in enumerate(nlp.pipe(texts, batch_size=batch_size)):
        topic = topics[i]
        if doc is None:
            continue

        # Collect lemmas we keep
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

        L = len(lemmas)
        for n in range(min_ngram, max_ngram + 1):
            if n <= 0 or L < n:
                continue
            for j in range(L - n + 1):
                term = sep.join(lemmas[j:j+n])
                counts[(str(topic), term)] += 1

    # Convert to list-of-dicts for reticulate
    out = []
    for (topic, term), n in counts.items():
        out.append({'topic': topic, 'term': term, 'n': int(n)})

    return out
"
  reticulate::py_run_string(py_code, convert = FALSE)

  # --- Call Python ------------------------------------------------------
  texts  <- as.character(df$text)
  # Coerce topics to character for stable Python dict keys + later joins
  topics <- as.character(df$topic)

  res <- reticulate::py$parse_and_count_terms(
    texts   = texts,
    topics  = topics,
    model   = spacy_model,
    pos_keep = pos_keep,
    min_char = as.integer(min_char),
    min_ngram = as.integer(min_ngram),
    max_ngram = as.integer(max_ngram),
    sep = ngram_sep,
    batch_size = as.integer(128)
  )

  res_r <- reticulate::py_to_r(res)

  if (length(res_r) == 0) {
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

  term_counts_full <- dplyr::bind_rows(res_r) %>%
    dplyr::mutate(
      topic = as.character(topic),
      term  = as.character(term),
      n     = as.integer(n)
    ) %>%
    dplyr::filter(!is.na(topic), !is.na(term), term != "")

  # --- Stopwords (exact-match, as in your original design) --------------
  if (!is.null(stopwords)) {
    sw <- tolower(stopwords)
    term_counts_full <- term_counts_full %>%
      dplyr::filter(!term %in% sw)
  }

  if (nrow(term_counts_full) == 0) {
    warning("No terms left after filtering; check stopwords / filters.")
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
  top_terms_primary <- term_scores_full %>%
    dplyr::group_by(topic) %>%
    dplyr::filter(n >= min_term_freq) %>%
    dplyr::arrange(dplyr::desc(tfidf), dplyr::desc(n), .by_group = TRUE) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::ungroup()

  topics_all      <- unique(term_scores_full$topic)
  topics_with_any <- unique(top_terms_primary$topic)
  topics_missing  <- setdiff(topics_all, topics_with_any)

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
