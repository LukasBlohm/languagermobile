#' vocab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_vocab_ui <- function(id){
  ns <- NS(id)

  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("language"), "Choose Language:", choices = c("EN", "FR")),
        radioButtons(ns("word_source"), "Word Source:",
                     choices = c("Random", "User Choice"),
                     selected = "Random"),
        conditionalPanel(
          condition = "output.user_word",
          ns = ns,
          textInput(ns("user_word_to_translate"), "Enter word:")
        ),
        conditionalPanel(
          condition = "!output.user_word",
          ns = ns,
          actionButton(ns("btn_get_word"), "Sample a word")
        ),
        textInput(ns("guess"), "Your Guess:"),
        actionButton(ns("submit"), "Submit"),
        tags$script(HTML(submit_on_enter(btn_id = ns("submit")))),
      ),
      mainPanel(
        uiOutput(ns("word_display")),

        # actionButton(ns("btn_next"), "Next"),
        textOutput(ns("feedback"))
      )
      )
  )
}

#' vocab Server Functions
#'
#' @noRd
mod_vocab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Read the vocab data
    vocab_data <- reactive({
      read.csv("vocab_1000.csv")
    })

    list_reactives <- reactiveValues(
      user_word = FALSE
    )

    output$user_word <- reactive({
      list_reactives$user_word
    })
    outputOptions(
      output, "user_word", suspendWhenHidden = FALSE
    )

    observe({
      if (input$word_source == "User Choice") {
        message("Set input source to User Choice")
        list_reactives$user_word <- TRUE
      } else {
        message("Set input source to Random")
        list_reactives$user_word <- FALSE
      }
    })

    observe({
      message("word_to_translate: ")
      print(word_to_translate())
    })

    # Randomly sample or let the user choose a word
    # word_to_translate <- eventReactive(input$btn_get_word, {
    #   if (input$word_source == "Random") {
    #     message("Sample a random word")
    #     sample(vocab_data()[, input$language], 1)
    #   } else {
    #     message("Register user submission")
    #     input$user_word_to_translate
    #   }
    # })
    word_to_translate <- eventReactive(input$btn_get_word, {
      if (input$word_source == "Random") {
        message("Sample a random word")
        sample(vocab_data()[, input$language], 1)
      } else {
        message("Register user submission")
        input$user_word_to_translate
      }
    })

    # Display the word to the user
    output$word_display <- renderUI({
      message("Show word to translate")
      word_to_translate()
    })

    # Provide feedback on the user's guess
    observeEvent(input$submit, {

      real_translation <- vocab_data()[vocab_data()[, input$language] == word_to_translate(), "FR"]
      if (input$guess == real_translation) {
        message("Correct submission")
        output$feedback <- renderText({"Correct!"})

        # Clear text input
        updateTextInput(session, "guess", value = "")

      } else {
        output$feedback <- renderText({
          message("Incorrect submission")
          paste0("Incorrect. The actaul translation is ", real_translation)
          })
      }


    })
  })
}

