#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  mod_vocab_server("vocab_module")
  mod_idiom_server("idiom_module")
  mod_sentence_server("sentence_module")
}
