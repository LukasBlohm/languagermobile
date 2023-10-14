#' idiom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_idiom_ui <- function(id){
  ns <- NS(id)

  tagList(
    selectInput(ns("language_1"),
                "Choose Language:",
                choices = c("DE", "FR"),
                selected = "FR"),

    actionButton(ns("btn_load"), "Show new idiom"),
    actionButton(ns("btn_show_result"), "Show translation"),

    # textOutput(ns("feedback")),
    tags$script(HTML(submit_on_enter(btn_id = ns("btn_show_result")))),
    tags$script(HTML(submit_on_enter(btn_id = ns("btn_load")))),


    br(),
    br(),
    uiOutput(ns("idiom_language1")),
    br(),
    uiOutput(ns("idiom_language2")),

  )
}

#' idiom Server Functions
#'
#' @noRd
mod_idiom_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Read the idiom data
    df_idioms <- reactive({
      read.csv("idioms.csv")
    })

    list_reactives <- reactiveValues(
      other_language = "DE",
      show_translation = FALSE
    )

    observe({
      # print(input$language_1)
      if (input$language_1 == "FR") {
        message("Language 1 set to FR, language 2 set to DE")
        list_reactives$other_language <- "DE"
      } else {
        message("Language 1 set to DE, language 2 set to FR")
        list_reactives$other_language <- "FR"
      }
    })

    # observe({
    #   message("idiom_to_translate: ")
    #   print(idiom_to_translate())
    # })

    idiom_to_translate <- eventReactive(input$btn_load, {
      message("Sample a random idiom")
      # print(df_idioms()[, input$language_1])
      list_reactives$show_translation <- FALSE
      sample(x = df_idioms()[, input$language_1], size = 1)
    })

    # Display the idiom to the user
    output$idiom_language1 <- renderUI({
      # message("btn_show_result idiom to translate")
      idiom_to_translate()
    })

    observeEvent(input$btn_show_result, {
      list_reactives$show_translation <- TRUE
    })


    # Show the translation
    observe({

      # print(paste0("list_reactives$show_translation: ", list_reactives$show_translation))

      if (list_reactives$show_translation) {
        message("Show translation")
        output$idiom_language2 <- renderText({
          df_idioms()[df_idioms()[[input$language_1]] == idiom_to_translate(),
                      list_reactives$other_language]
        })
      } else {
        message("Hide translation")
        output$idiom_language2 <- renderText({""})
      }
    })

  })
}
