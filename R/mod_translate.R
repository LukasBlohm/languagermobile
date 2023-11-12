#' translate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_translate_ui <- function(id){
  ns <- NS(id)

  tagList(
    br(),
    fluidRow(
      column(width = 4,
             selectInput(ns("language_1"),
                         "Choose Language:",
                         choices = c("EN", "FR"),
                         selected = "FR")
      )
    ),

    actionButton(ns("btn_load_deps"), "Load model"),

    textInput(ns("text_to_translate"), "Enter text to translate"),

    br(),

    fluidRow(
      column(width = 4,
             actionButton(ns("btn_translate"), "Show translation")
      )
    ),

    # textOutput(ns("feedback")),
    tags$script(HTML(submit_on_enter(btn_id = ns("btn_translate")))),

    br(),
    br(),
    textOutput(ns("text_translated")),

  )
}

#' translate Server Function
#'
#' @noRd
mod_translate_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # observeEvent(input$btn_load_deps, {
    #
    #   deps <- setup_translator(model_path = NULL, language_pairs = "fr-de")
    #
    # })

    deps <- setup_translator(model_path = NULL, language_pairs = "de-fr")

    list_reactives <- reactiveValues(
      other_language = "EN",
      show_translation = FALSE
    )


    observeEvent(input$btn_translate, {

      list_reactives$text_translated <- translate(
        dependencies = deps, language_pair = "de-fr",
        input_text = input$text_to_translate,
        character_cut = 1200, verbose = TRUE)},
      ignoreInit = TRUE)


    output$text_translated <- renderText({
      list_reactives$text_translated
    })

  })
}









