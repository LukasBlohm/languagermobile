#' sentence UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_explore_ui <- function(id){
  ns <- NS(id)

  tagList(

    tags$script('$(document).ready(function() { $("#someInput").focus(); });'),

    selectInput(ns("language_1"),
                "Choose Language:",
                choices = c("EN", "FR"),
                selected = "FR"),

    actionButton(ns("btn_load"), "Show new expression"),
    actionButton(ns("btn_show_result"), "Show translation"),

    # textOutput(ns("feedback")),
    tags$script(HTML(submit_on_enter(btn_id = ns("btn_show_result")))),


    br(),
    br(),
    uiOutput(ns("expression_language1")),
    br(),
    uiOutput(ns("expression_language2")),
    br(),
    uiOutput(ns("example_language1")),
    br(),
    uiOutput(ns("example_language2")),

    # bind_keys_to_buttons(c(13, 39), c("btn_show_result", "btn_load"))


  )
}

#' sentence Server Functions
#'
#' @noRd
mod_explore_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Read the sentence data
    df_active <- reactive({
      read.csv("data_inputs/argot.csv")
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
      # print(df_active()[, input$language_1])
      list_reactives$show_translation <- FALSE
      sample(x = df_active()[, input$language_1], size = 1)
    })

    # Display the sentence to the user
    output$expression_language1 <- renderUI({
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
        tr <- df_active()[df_active()[[input$language_1]] == sentence_to_translate(), list_reactives$other_language]

        message("Translation: ", tr)
        output$expression_language2 <- renderText({tr})


        example_selected_language <- df_active()[df_active()[[input$language_1]] == sentence_to_translate(), paste0("Example_", input$language_1)]

        example_other_language <- df_active()[df_active()[[input$language_1]] == sentence_to_translate(), paste0("Example_", list_reactives$other_language)]

        message("Example 1: ", example_selected_language)
        output$example_language1 <- renderText({example_selected_language})
        message("Example 2: ", example_other_language)
        output$example_language2 <- renderText({example_other_language})
      } else {
        message("Hide translation")
        output$expression_language2 <- renderText({""})
        output$example_language1 <- renderText({""})
        output$example_language2 <- renderText({""})
      }
    })
  })
}




