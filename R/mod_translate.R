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
                         "Input Language",
                         choices = c("DE"),
                         selected = "DE")
      ),
      column(width = 4,
             selectInput(ns("language_2"),
                         "Output Language",
                         choices = c("FR"),
                         selected = "FR")
      )
    ),

    # actionButton(ns("btn_load_dependencies"), "Load model"),

    textInput(ns("text_to_translate"), "Enter text to translate"),

    br(),
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
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    list_reactives <- reactiveValues(
      text_translated = ""
      )

    dependencies <- setup_translator(
      # model_path = NULL,
      language_pairs = "de-fr"  # Can be expanded to all installed models
      )

    observeEvent(input$btn_translate, {

      list_reactives$text_translated <- translate(
        dependencies = dependencies,
        language_pair = paste0(tolower(input$language_1), "-",
                               tolower(input$language_2)),
        input_text = input$text_to_translate, verbose = TRUE)},
      ignoreInit = TRUE)


    output$text_translated <- renderText({
      list_reactives$text_translated
    })

  })
}









