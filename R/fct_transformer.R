.python_modules <- new.env()

#' Load dependencies for an individual model
#'
#' @param model_path Path to the model. If null, the remote is used.
#' @param language_pair
#' @param transformers
#'
#' @noRd
#'
#' @return A list containing the model and tokenizer for one language pair
#'
#' @examples
load_dependencies <- function(model_path, language_pair = "fr-de", transformers) {

  # .python_modules$transformers <- reticulate::import("transformers")
  #
  # transformers <- .python_modules$transformers

  # language_pair <- paste0(language_in, "-", language_out)

  if (!is.null(model_path)) {
    tokenizer <- transformers$MarianTokenizer$from_pretrained(
      file.path(model_path, paste0("model_", language_pair))
    )

    model <- transformers$MarianMTModel$from_pretrained(
      file.path(model_path, paste0("model_", language_pair))
    )
  } else {
    tokenizer <- transformers$MarianTokenizer$from_pretrained(
      paste0("Helsinki-NLP/opus-mt-", language_pair)
    )
    model <- transformers$MarianMTModel$from_pretrained(
      paste0("Helsinki-NLP/opus-mt-", language_pair)
      )
  }
  message("Dependencies for ", language_pair, " loaded.")

  return(list(
    tokenizer = tokenizer, model = model
  ))
}


#' Setup_translator
#'
#' @param model_path
#' @param language_in
#' @param language_out
#'
#' @importFrom magrittr %>%
#'
#' @noRd
#'
#' @return A list containing the model and tokenizer for the language pairs
#'
#' @examples
setup_translator <- function(language_pairs = "fr-de", model_path = NULL) {

  .python_modules$transformers <- reticulate::import("transformers")

  dependencies <- purrr::map(
    language_pairs,
    ~ load_dependencies(language_pair = .x, model_path = model_path,
                        transformers = .python_modules$transformers)
    ) %>%
    purrr::set_names(language_pairs)

  return(dependencies)
}



#' Translate
#'
#' @param dependencies
#' @param language_pair
#' @param input_text
#' @param character_cut
#' @param verbose
#'
#' @importFrom magrittr %>%
#'
#' @noRd
#'
#' @return The translation of input_text
#'
#' @examples
translate <- function(dependencies, language_pair, input_text, character_cut = 1200, verbose = TRUE) {
  tokenizer <- dependencies[[language_pair]][["tokenizer"]]
  model <- dependencies[[language_pair]][["model"]]

  start_time <- Sys.time()

  n_characters <- nchar(input_text)

  if (!is.na(input_text) & n_characters > 0) {
    input_tokens <- tokenizer$encode(input_text, return_tensors = "pt")

    if (n_characters < character_cut) {
      input_tokens <- tokenizer$encode(input_text, return_tensors = "pt")

      outputs <- model$generate(input_tokens, max_new_tokens = 500)
      translated_text <- tokenizer$decode(outputs[[0]], skip_special_tokens = TRUE)

      if (verbose) {
        message("Normal translation finished (",
                n_characters, " characters in ",
                lubridate::dseconds(round(
                  lubridate::as.duration(Sys.time() - start_time), 1)
                  ), ")")
      }

      return(translated_text)
    } else {

      if (verbose) {
        message("Text too long")
      }
      return("")
    }
  }
}




















c
