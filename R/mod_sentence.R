#' sentence UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sentence_ui <- function(id){
  ns <- NS(id)

  tagList(
    selectInput(ns("language_1"),
                "Choose Language:",
                choices = c("EN", "FR"),
                selected = "FR"),

    actionButton(ns("btn_load"), "Show new sentence"),
    actionButton(ns("btn_show_result"), "Show translation"),

    # textOutput(ns("feedback")),
    tags$script(HTML(submit_on_enter(btn_id = ns("btn_show_result")))),
    tags$script(HTML(submit_on_enter(btn_id = ns("btn_load")))),


    br(),
    br(),
    uiOutput(ns("sentence_language1")),
    br(),
    uiOutput(ns("sentence_language2")),

  )
}

#' sentence Server Functions
#'
#' @noRd
mod_sentence_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Read the sentence data
    df_sentences <- reactive({
      read.csv("sentences.csv")
    })

    list_reactives <- reactiveValues(
      other_language = "EN",
      show_translation = FALSE
    )

    observe({
      # print(input$language_1)
      if (input$language_1 == "FR") {
        message("Language 1 set to FR, language 2 set to EN")
        list_reactives$other_language <- "EN"
      } else {
        message("Language 1 set to EN, language 2 set to FR")
        list_reactives$other_language <- "FR"
      }
    })

    # observe({
    #   message("sentence_to_translate: ")
    #   print(sentence_to_translate())
    # })

    sentence_to_translate <- eventReactive(input$btn_load, {
      message("Sample a random sentence")
      # print(df_sentences()[, input$language_1])
      list_reactives$show_translation <- FALSE
      sample(x = df_sentences()[, input$language_1], size = 1)
    })

    # Display the sentence to the user
    output$sentence_language1 <- renderUI({
      # message("btn_show_result sentence to translate")
      sentence_to_translate()
    })

    observeEvent(input$btn_show_result, {
      list_reactives$show_translation <- TRUE
    })


    # Show the translation
    observe({

      # print(paste0("list_reactives$show_translation: ", list_reactives$show_translation))

      if (list_reactives$show_translation) {
        message("Show translation")
        tr <- df_sentences()[df_sentences()[[input$language_1]] == sentence_to_translate(),
                              list_reactives$other_language]

        message("Translation: “", tr)
        output$sentence_language2 <- renderText({tr})
      } else {
        message("Hide translation")
        output$sentence_language2 <- renderText({""})
      }
    })
  })
}
