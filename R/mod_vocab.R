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
        selectInput(ns("language_1"), "Original Language", choices = c("EN", "FR")),
        radioButtons(ns("word_source"), "Word Source:",
                     choices = c("Random", "User Choice"),
                     selected = "Random"),
        conditionalPanel(
          condition = "output.user_word",
          ns = ns,
          textInput(ns("user_word_to_translate"), "Enter word")
        ),
        conditionalPanel(
          condition = "!output.user_word",
          ns = ns,
          actionButton(ns("btn_get_word_to_translate"), "Sample a word")
        ),
        br(),
        textInput(ns("guess"), "Your Guess"),
        actionButton(ns("submit"), "Submit"),
        tags$script(HTML(submit_on_enter(btn_id = ns("submit")))),
      ),
      mainPanel(
        # uiOutput(ns("word_display")),
        tableOutput(ns("word_display")),
        uiOutput(ns("feedback"))
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
      read.csv("data_inputs/vocab_1000.csv")
    })

    list_reactives <- reactiveValues(
      user_word = FALSE,
      other_language = "EN"
    )
    word_to_translate <- reactiveVal("")

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

    # observe({
    #   message("Word to translate: ", word_to_translate())
    # })

    observeEvent(input$btn_get_word_to_translate, {
      message("Sample a random word")
      word_to_translate(
        sample(vocab_data()[, input$language_1], 1)
      )
    })

    output$word_display <- renderTable({
      # message("Show word to translate")
      data.frame(Word = word_to_translate())
    }, width = "60%")

    observeEvent(input$btn_get_word_to_translate, {
      output$word_display <- renderTable({
        message("Show word to translate")
        data.frame(Word = word_to_translate())
      }, width = "60%")
      output$feedback <- renderText({
        ""
      })
    })


    observeEvent(input$language_1, {
      word_to_translate("")
    })

    # Provide feedback on the user's guess
    observeEvent(input$submit, {

      if (input$word_source != "Random") {
        message("Register user submission")
        # list_reactives$show_translation <- FALSE
        word_to_translate(
          input$user_word_to_translate
        )
      }

      req(word_to_translate())  # Prevent error when submit is pressed but no sentence has been sampled yet.

      real_translation <- vocab_data()[vocab_data()[, input$language_1] == word_to_translate(), list_reactives$other_language]

      # print(paste0("input$guess: ", input$guess))
      # print(paste0("real_translation: ", real_translation))

      message("Correct submission = ", input$guess %in% real_translation) # there can be more than 1 correct translation, e.g. temps = time and weather

      if (input$guess %in% real_translation) {
        message("Correct submission")
        output$feedback <- renderText({"Correct!"})

        # Clear text input
        updateTextInput(session, "guess", value = "")

        output$word_display <- renderTable({
          message("Show word to translate")
          data.frame(Word = word_to_translate(),
                     Translation = real_translation)
        }, width = "60%")

      } else {
        output$feedback <- renderText({
          message("Incorrect submission")
          paste0("Incorrect. The actaul translation is ", real_translation)
          })

        output$word_display <- renderTable({
          message("Show word to translate")
          data.frame(Word = word_to_translate())
        }, width = "60%")
      }
    })
  })
}

