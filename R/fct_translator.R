.python_modules <- new.env()


#' Setup_translator
#'
#' This function prepares everything that is needed to conduct translations
#' with the specified language pairs.
#'
#' @param language_pairs Character vector, specifying the pairs of languages to be
#' translated. E.g. `"de-fr"` or `c("de-fr", "en-fr")`
#' @param model_path Path to the model. If null, the default cache folder
#' (e.g. `~/.cache/huggingface/hub/`) is used.
#'
#' @return A list containing the model and tokenizer for the language pairs
#'
#' @examples
#' \dontrun{setup_translator()}
setup_translator <- function(language_pairs = "de-fr", model_path = NULL) {

  message("Start setup of reticulate-python")
  setup_virtualenv()

  message("Module import")
  .python_modules$transformers <- reticulate::import("transformers")

  message("Load dependencies")
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
#' This function translates a particular string.
#'
#' @param input_text String of the text to be translated
#' @param dependencies Object storing the model and tokenizer for `language_pair.`.
#'  Output of [setup_translator()]
#' @param language_pair String, specifying the pair of languages to be translated.
#'  E.g. `"de-fr"`
#' @param verbose Logical, should messages be printed to the console?
#'
#' @return The translation of input_text
#'
#' @examples
#' \dontrun{translate(input_text = "Haus", dependencies = deps, language_pair = "de-fr")}
translate <- function(input_text, dependencies, language_pair, verbose = TRUE) {
  tokenizer <- dependencies[[language_pair]][["tokenizer"]]
  model <- dependencies[[language_pair]][["model"]]

  start_time <- Sys.time()

  n_characters <- nchar(input_text)

  if (!is.na(input_text) & n_characters > 0) {
    input_tokens <- tokenizer$encode(input_text, return_tensors = "pt")

    if (n_characters < 1200) {
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
