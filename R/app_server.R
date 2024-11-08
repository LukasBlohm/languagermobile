#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
app_server <- function(input, output, session) {

  mod_explore_server("explore_module")
  mod_vocab_server("vocab_module")

  # shiny::observeEvent(input$tabs, {
  #   if (input$tabs == "Explorer") {
  #     cli::cli_h1("Load Explorer Module")
  #     mod_explore_server("explore_module")
  #   }
  # })
  #
  # shiny::observeEvent(input$tabs, {
  #   if (input$tabs == "Vocab") {
  #     cli::cli_h1("Load Vocab Module")
  #     mod_vocab_server("vocab_module")
  #   }
  # })
  #
  # # Activate translator module once the user navigates there
  # shiny::observeEvent(input$tabs, {
  #   if (input$tabs == "Translator") {
  #     cli::cli_h1("Load Translator Module")
  #     mod_translate_server("translate_module")
  #   }
  # })
}
