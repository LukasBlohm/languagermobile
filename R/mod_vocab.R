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

  bslib::page_sidebar(

    sidebar = bslib::sidebar(
      shiny::selectInput(ns("language_1"), "Original Language", choices = c("EN", "FR")),
      shiny::radioButtons(ns("word_source"), "Word Source:",
                          choices = c("Random", "User Choice"),
                          selected = "Random"),
      shiny::conditionalPanel(
        condition = "output.user_word",
        ns = ns,
        shiny::textInput(ns("user_word_to_translate"), "Enter word")
      ),
      shiny::conditionalPanel(
        condition = "!output.user_word",
        ns = ns,
        shiny::actionButton(ns("btn_get_word_to_translate"), "Sample a word")
      ),
      htmltools::br(),
      shiny::textInput(ns("guess"), "Your Guess"),
      shiny::actionButton(ns("submit"), "Submit"),
      tags$script(htmltools::HTML(submit_on_enter(btn_id = ns("submit")))),
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Word"),
      shiny::tableOutput(ns("word_display")),
      shiny::uiOutput(ns("feedback"))
    )
    # ,
    # bslib::card(
    #   full_screen = TRUE,
    #   # shinybrowser::detect(),
    #   "Are you on mobile?",
    #   shiny::textOutput(ns("result"))
    # )
  )
}

#' vocab Server Functions
#'
#' @noRd
mod_vocab_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observe({
      if (isTRUE(shiny::isolate(shinybrowser::is_device_mobile()))) {
        cli::cli_alert_info("Mobile")
      } else {
        cli::cli_alert_info("Desktop")
      }
    })
    output$result <- shiny::renderText({
      shinybrowser::is_device_mobile()
    })
    # if (isTRUE(shinybrowser::is_device_mobile())) {
    #   message("Mobile")
    # } else {
    #   message("Desktop")
    # }


    vocab_data <- .GlobalEnv$df_vocab

    list_reactives <- shiny::reactiveValues(
      user_word = FALSE,
      other_language = "EN"
    )
    word_to_translate <- shiny::reactiveVal("")

    shiny::observe({
      # print(input$language_1)
      if (input$language_1 == "FR") {
        cli::cli_alert_info("Language 1 set to FR, language 2 set to EN")
        list_reactives$other_language <- "EN"
      } else {
        cli::cli_alert_info("Language 1 set to EN, language 2 set to FR")
        list_reactives$other_language <- "FR"
      }
    })

    output$user_word <- shiny::reactive({
      list_reactives$user_word
    })
    shiny::outputOptions(
      output, "user_word", suspendWhenHidden = FALSE
    )

    shiny::observe({
      if (input$word_source == "User Choice") {
        cli::cli_alert_info("Set input source to User Choice")
        list_reactives$user_word <- TRUE
      } else {
        cli::cli_alert_info("Set input source to Random")
        list_reactives$user_word <- FALSE
      }
    })

    # observe({
    #   message("Word to translate: ", word_to_translate())
    # })

    shiny::observeEvent(input$btn_get_word_to_translate, {
      word_to_translate(
        dplyr::pull(dplyr::slice_sample(vocab_data[, input$language_1], n = 1))
      )
      cli::cli_alert("Sampled random word {word_to_translate()}")
    })


    output$word_display <- shiny::renderTable({
      data.frame(Word = word_to_translate())
    }, width = "60%")

    shiny::observeEvent(input$btn_get_word_to_translate, {
      output$word_display <- shiny::renderTable({
        cli::cli_text("Show word to translate")
        data.frame(Word = word_to_translate())
      }, width = "60%")
      output$feedback <- shiny::renderText("")
    })



    shiny::observeEvent(input$language_1, {
      word_to_translate("")
    })

    # Provide feedback on the user's guess
    shiny::observeEvent(input$submit, {

      if (input$word_source != "Random") {
        cli::cli_text("Register user submission")
        # list_reactives$show_translation <- FALSE
        word_to_translate(
          input$user_word_to_translate
        )
      }

      shiny::req(word_to_translate())  # Prevent error when submit is pressed but no sentence has been sampled yet.

      real_translation <- vocab_data[vocab_data[, input$language_1] == word_to_translate(), list_reactives$other_language]

      # print(paste0("input$guess: ", input$guess))
      # print(paste0("real_translation: ", real_translation))

      # cli::cli_alert("Correct submission = {input$guess %in% real_translation}") # there can be more than 1 correct translation, e.g. temps = time and weather

      if (input$guess %in% real_translation) {
        cli::cli_alert_success("Correct submission")
        output$feedback <- shiny::renderText({"Correct!"})

        # Clear text input
        shiny::updateTextInput(session, "guess", value = "")

        output$word_display <- shiny::renderTable({
          cli::cli_alert("Show word to translate")
          data.frame(Word = word_to_translate(),
                     Translation = real_translation)
        }, width = "60%")

      } else {
        output$feedback <- shiny::renderText({
          cli::cli_alert("Incorrect submission")
          paste0("Incorrect. The actual translation is ", real_translation)
          })

        output$word_display <- shiny::renderTable({
          cli::cli_alert("Show word to translate")
          data.frame(Word = word_to_translate())
        }, width = "60%")
      }
    })
  })
}

