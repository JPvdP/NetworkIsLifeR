#' Compute topic representation terms using udpipe + quanteda (R-only)
#'
#' @param data A data.frame with at least three columns: doc id, topic, text.
#' @param doc_col Name of the document ID column (unquoted).
#' @param topic_col Name of the topic column (unquoted).
#' @param text_col Name of the text column (unquoted).
#' @param ud_model A loaded udpipe model (see udpipe::udpipe_download_model()
#'   and udpipe::udpipe_load_model()).
#' @param use_lemma If TRUE, use udpipe lemmas, otherwise surface tokens.
#' @param max_ngram Maximum n-gram length (e.g. 3 for 1-3 grams).
#' @param min_term_freq Minimum total frequency of a term in the corpus
#'   to keep it (integer >= 1).
#' @param allowed_upos Coarse POS tags to keep before making n-grams
#'   (default: c("NOUN", "PROPN", "ADJ")).
#'
#' @return A data.frame with one row per (topic, term) and columns:
#'   topic, term, tf, tf_norm, tf_idf, c_tf_idf, lift, df_corpus,
#'   tf_corpus, total_tf_topic, n_topics_with_term.
#'
#' @examples
#' \dontrun{
#' udpipe::udpipe_download_model('english')
#' model <- udpipe::udpipe_load_model()
#' compute_topic_terms_udpipe(df, ud_model = model)
#' }
## End(**Not run**)
#' @export
compute_topic_terms_udpipe <- function(
    data,
    doc_col,
    topic_col,
    text_col,
    ud_model,
    use_lemma     = TRUE,
    max_ngram     = 3L,
    min_term_freq = 5L,
    allowed_upos  = c("NOUN", "PROPN", "ADJ")
) {
  # ---- Dependencies ---------------------------------------------------------
  pkgs <- c("udpipe", "quanteda", "dplyr", "Matrix", "rlang", "stringr")
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing, collapse = ", "),
         call. = FALSE)
  }

  if (!inherits(ud_model, "udpipe_model")) {
    stop("Argument 'ud_model' must be a loaded udpipe model.\n",
         "Use udpipe::udpipe_download_model() and udpipe::udpipe_load_model().",
         call. = FALSE)
  }

  # Tidy-eval set-up
  doc_col_sym   <- rlang::ensym(doc_col)
  topic_col_sym <- rlang::ensym(topic_col)
  text_col_sym  <- rlang::ensym(text_col)

  # ---- Prepare input --------------------------------------------------------
  df <- data %>%
    dplyr::select(
      doc_id = !!doc_col_sym,
      topic  = !!topic_col_sym,
      text   = !!text_col_sym
    )

  # ensure simple types
  df$doc_id <- as.character(df$doc_id)
  df$topic  <- as.character(df$topic)
  df$text   <- as.character(df$text)

  # Store doc-topic mapping (unique)
  doc_topics <- df %>%
    dplyr::select(doc_id, topic) %>%
    dplyr::distinct()

  # ---- UDPIPE annotation ----------------------------------------------------
  anno <- udpipe::udpipe(
    x      = df$text,
    doc_id = df$doc_id,
    object = ud_model
  )
  # anno columns include: doc_id, paragraph_id, sentence_id, token_id,
  # token, lemma, upos, etc.

  # Choose token form: lemma or surface
  if (use_lemma) {
    anno$term_token <- anno$lemma
  } else {
    anno$term_token <- anno$token
  }

  anno <- anno %>%
    dplyr::filter(
      !is.na(term_token),
      term_token != "",
      upos %in% allowed_upos
    ) %>%
    dplyr::arrange(doc_id, paragraph_id, sentence_id, token_id)

  # Collapse kept tokens back into "noun-phrase-like" streams per doc
  noun_text <- anno %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(
      text_np = paste(term_token, collapse = " "),
      .groups = "drop"
    )

  # Attach topics and handle docs with no kept tokens
  doc_np <- doc_topics %>%
    dplyr::left_join(noun_text, by = "doc_id") %>%
    dplyr::mutate(
      text_np = ifelse(is.na(text_np), "", text_np)
    )

  # ---- Quanteda tokens + n-grams --------------------------------------------
  toks <- quanteda::tokens(
    doc_np$text_np,
    what          = "word",
    remove_punct  = TRUE,
    remove_numbers = TRUE
  )

  # Create 1..max_ngram n-grams from these tokens
  toks_ng <- quanteda::tokens_ngrams(
    toks,
    n            = 1:max_ngram,
    concatenator = "_"  # safer inside quanteda; we'll replace later for display
  )

  # Attach docnames and docvars
  quanteda::docnames(toks_ng) <- doc_np$doc_id
  quanteda::docvars(toks_ng, "topic") <- doc_np$topic

  # Document-feature matrix (docs x terms)
  dfm_docs <- quanteda::dfm(toks_ng)

  # Remove empty docs (no terms after filtering)
  keep_docs <- Matrix::rowSums(dfm_docs) > 0
  dfm_docs  <- dfm_docs[keep_docs, ]

  if (quanteda::nfeat(dfm_docs) == 0) {
    warning("No terms remaining after udpipe filtering and n-gram generation.")
    return(data.frame(
      topic = character(0),
      term  = character(0)
    ))
  }

  # ---- Term frequency filter -------------------------------------------------
  if (!is.null(min_term_freq) && min_term_freq > 1L) {
    term_totals <- Matrix::colSums(dfm_docs)
    keep_terms  <- names(term_totals)[term_totals >= min_term_freq]
    dfm_docs    <- dfm_docs[, keep_terms]
  }

  if (quanteda::nfeat(dfm_docs) == 0) {
    warning("All terms were removed by 'min_term_freq' filter.")
    return(data.frame(
      topic = character(0),
      term  = character(0)
    ))
  }

  # ---- Basic corpus stats ----------------------------------------------------
  N_docs    <- quanteda::ndoc(dfm_docs)
  df_corpus <- quanteda::docfreq(dfm_docs)         # docs where term appears

  # ---- Group by topic (class-based view) ------------------------------------
  dfm_topic <- quanteda::dfm_group(
    dfm_docs,
    groups = quanteda::docvars(dfm_docs, "topic")
  )
  topics    <- rownames(dfm_topic)
  N_topics  <- quanteda::ndoc(dfm_topic)

  # Sparse matrices
  tf_topic_mat        <- dfm_topic                       # topics x terms
  total_tokens_topic  <- Matrix::rowSums(tf_topic_mat)   # per topic
  topics_with_term    <- quanteda::docfreq(dfm_topic)    # in how many topics term appears
  tf_corpus           <- Matrix::colSums(tf_topic_mat)   # total term count across topics
  total_tokens_corpus <- sum(tf_corpus)

  # How many docs per topic?
  n_docs_in_topic <- table(doc_np$topic)
  n_docs_in_topic <- as.numeric(n_docs_in_topic)
  names(n_docs_in_topic) <- names(table(doc_np$topic))

  # ---- Convert topic-term matrix to long format ------------------------------
  smry <- Matrix::summary(tf_topic_mat)
  # smry has i (row index), j (col index), x (count)

  out <- data.frame(
    topic = topics[smry$i],
    term  = colnames(tf_topic_mat)[smry$j],
    tf    = as.numeric(smry$x),
    stringsAsFactors = FALSE
  )

  # Join in metrics using vector lookups
  out$total_tf_topic <- total_tokens_topic[ match(out$topic, topics) ]
  out$df_corpus      <- as.numeric(df_corpus[ out$term ])
  out$tf_corpus      <- as.numeric(tf_corpus[ out$term ])
  out$n_topics_with_term <- as.numeric(topics_with_term[ out$term ])
  out$n_docs_in_topic <- n_docs_in_topic[ out$topic ]

  # Avoid division by zero
  eps <- 1e-9

  # Normalised TF within topic
  out$tf_norm <- out$tf / (out$total_tf_topic + eps)

  # Classic TF-IDF: use topic-level tf and corpus document frequency
  out$tf_idf <- out$tf * log(N_docs / (1 + out$df_corpus))

  # Class-based TF-IDF (topic as "class", document-based version)
  # c_tf_idf = (tf / n_docs_in_topic) * log(N_topics / (1 + n_topics_with_term))
  out$c_tf_idf <- (out$tf / (out$n_docs_in_topic + eps)) *
    log(N_topics / (1 + out$n_topics_with_term))

  # Lift: how overrepresented term is in topic vs whole corpus
  out$p_topic_term  <- out$tf / (out$total_tf_topic + eps)
  out$p_corpus_term <- out$tf_corpus / (total_tokens_corpus + eps)
  out$lift <- out$p_topic_term / (out$p_corpus_term + eps)

  # Make terms prettier: replace "_" (quanteda n-gram joiner) with space
  out$term <- stringr::str_replace_all(out$term, "_", " ")

  # Order by topic and descending c_tf_idf
  out <- out %>%
    dplyr::arrange(.data$topic, dplyr::desc(.data$c_tf_idf))

  rownames(out) <- NULL
  out
}
