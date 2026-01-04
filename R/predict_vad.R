#' Predict Valence–Arousal–Dominance (VAD) scores with a fine-tuned BERT model
#'
#' Computes continuous emotion scores along the three-dimensional
#' Valence–Arousal–Dominance (VAD) framework for one or more input texts using a
#' Hugging Face Transformers sequence classification model (e.g.,
#' \code{"RobroKools/vad-bert"}).
#'
#' The function assumes that \code{tokenizer} and \code{model} are already loaded
#' and available in the calling environment (for example via \code{huggingfaceR}
#' or \code{reticulate} importing Python Transformers objects). Inputs are
#' tokenized with truncation enabled to reduce the risk of exceeding typical
#' transformer maximum sequence lengths (often 512 tokens).
#'
#' @param text A character vector of one or more texts. If a vector is provided,
#'   it is passed to the tokenizer in batch mode.
#'
#' @return A named numeric vector of length 3 with elements:
#'   \code{valence}, \code{arousal}, and \code{dominance}.
#'   If \code{text} is a character vector of length > 1, the returned values
#'   reflect the underlying model output as returned by the Python model call;
#'   depending on the tokenizer/model configuration this may require adapting
#'   the function to return a matrix/data frame (see Details).
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item Tokenize \code{text} with padding and truncation.
#'   \item Call the Transformer model using \code{input_ids} and
#'         \code{attention_mask} (and \code{token_type_ids} when present).
#'   \item Convert the resulting logits to an R numeric vector via
#'         \code{detach() -> cpu() -> numpy()}.
#' }
#'
#' \strong{Batch inputs:} Many tokenizer/model combinations return a
#' two-dimensional logits tensor for batched inputs (one row per text). The
#' current implementation coerces the logits to a flat numeric vector and
#' labels the first three values as VAD. If you intend to score multiple texts
#' at once, consider adapting the return type to a matrix or data frame, e.g.
#' one row per input text and three columns (V/A/D).
#'
#' \strong{Model objects:} \code{tokenizer} and \code{model} are not arguments to
#' this function. They must exist in scope (e.g., in your package environment or
#' set during package initialization).
#'
#' @examples
#' \dontrun{
#' # Example setup (run once):
#' library(huggingfaceR)
#' tokenizer <- hf_load_tokenizer("RobroKools/vad-bert")
#' model <- hf_load_AutoModel_for_task(
#'   model_type = "AutoModelForSequenceClassification",
#'   model_id   = "RobroKools/vad-bert"
#' )
#'
#' # Single text
#' predict_vad("I feel hopeful and in control.")
#' }
#'
#' @export
predict_vad <- function(text) {
  # Tokenize (keep truncation TRUE to avoid >512 token errors)
  inputs <- tokenizer(
    text,
    return_tensors = "pt",
    truncation = TRUE,
    padding = TRUE
  )

  # Call model with the tensors (avoid Python **kwargs syntax by passing explicitly)
  args <- list(
    input_ids      = inputs$input_ids,
    attention_mask = inputs$attention_mask
  )
  if (!is.null(inputs$token_type_ids)) args$token_type_ids <- inputs$token_type_ids

  outputs <- do.call(model, args)

  # logits -> numeric vector
  vad <- outputs$logits$detach()$cpu()$numpy()
  vad <- as.numeric(vad)

  setNames(vad, c("valence", "arousal", "dominance"))
}
