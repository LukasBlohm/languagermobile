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
    selectInput(ns("language"), "Choose Language:", choices = c("EN", "FR")),
    radioButtons(ns("word_source"), "Word Source:", choices = c("Random", "Choose")),
    uiOutput(ns("word_display")),
    textInput(ns("guess"), "Your Guess:"),
    actionButton(ns("submit"), "Submit"),
    textOutput(ns("feedback")),
    tags$script(HTML(submit_on_enter(btn_id = ns("submit"))))
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

    # Randomly sample or let the user choose a word
    random_word <- reactive({
      if (input$word_source == "Random") {
        sample(vocab_data()[, input$language], 1)
      } else {
        # Code to let user choose a word
      }
    })

    # Display the word to the user
    output$word_display <- renderUI({
      random_word()
    })

    # Provide feedback on the user's guess
    observeEvent(input$submit, {
      real_translation <- vocab_data()[vocab_data()[, input$language] == random_word(), "FR"]
      if (input$guess == real_translation) {
        output$feedback <- renderText({"Correct!"})
      } else {
        output$feedback <- renderText({
          paste0("Incorrect. The actaul translation is ", real_translation)
          })
      }
    })
  })
}

