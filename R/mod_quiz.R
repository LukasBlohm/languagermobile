#' quiz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_quiz_ui <- function(id){
  ns <- shiny::NS(id)

  bslib::page(

    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Word"),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::selectInput(
            ns("language_1"), "Original Language", choices = c("EN", "FR")
          )
        ),
        shiny::column(
          width = 6,
          shiny::selectInput(
            ns("language_2"),
            "Translation",
            choices = NULL,
            multiple = FALSE
          )
        )
      )
    ),

    bslib::card(
      shiny::actionButton(ns("btn_get_word_to_translate"), "Sample a word"),
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Word"),
      shiny::tableOutput(ns("word_display")),
      shiny::uiOutput(ns("feedback"))
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Your choice"),
      shinyMobile::f7Radio(
        ns("word_answer"), "Your answer",
        choices = rep("", 4)
        )
    )
    # ,
    # bslib::card(
    #   shiny::actionButton(ns("submit"), "Submit"),
    # )
  )
}

#' quiz Server Functions
#'
#' @noRd
mod_quiz_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    list_reactives <- shiny::reactiveValues(
      other_languages = character() # "EN"
    )
    word_to_translate <- shiny::reactiveVal("")


    shiny::observeEvent(colnames(.GlobalEnv$quiz_data$vocab_data), {

      shiny::req(nrow(.GlobalEnv$quiz_data$vocab_data) > 0)

      show_vector(colnames(.GlobalEnv$quiz_data$vocab_data))

      v_languages <- colnames(.GlobalEnv$quiz_data$vocab_data) %>%
        purrr::keep(~ .x %in% c("FR", "DE", "EN"))

      shiny::updateSelectInput(
        session = session,
        "language_1",
        choices = v_languages,
        selected = v_languages %>%
          purrr::discard(~ .x %in% c(list_reactives$other_languages))
      )
    })

    shiny::observe({

      shiny::updateSelectInput(
        session = session,
        "language_2",
        choices = list_reactives$other_languages,
        selected = NULL
      )
    })

    shiny::observe({

      shiny::req(nrow(.GlobalEnv$quiz_data$vocab_data) > 0)

      list_reactives$other_languages <- colnames(.GlobalEnv$quiz_data$vocab_data) %>%
        purrr::keep(~ .x %in% c("FR", "DE", "EN")) %>%
        purrr::discard(~ .x %in% c(input$language_1))

      other <- list_reactives$other_languages
      info("Language 1 set to {input$language_1}, {cli::qty(other)}other language{?/s} {?is/are} {other}.")
    })


    output$user_word <- shiny::reactive({
      list_reactives$user_word
    })
    shiny::outputOptions(
      output, "user_word", suspendWhenHidden = FALSE
    )


    shiny::observeEvent(input$btn_get_word_to_translate, {

      shiny::req(nrow(.GlobalEnv$quiz_data$vocab_data) > 0)

      word_to_translate(
        .GlobalEnv$quiz_data$vocab_data %>%
          dplyr::select(tidyselect::all_of(input$language_1)) %>%
          dplyr::slice_sample(n = 1) %>%
          dplyr::pull()
      )

      alert("Sampled random word {word_to_translate()}")

      v_options <- .GlobalEnv$quiz_data$vocab_data  %>%
        dplyr::select(tidyselect::all_of(input$language_2)) %>%
        dplyr::slice_sample(n = 4) %>%
        dplyr::pull()

      show_vector(v_options)

      shinyMobile::updateF7Radio(
        inputId = "word_answer", choices = v_options, session = session
        )
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
    shiny::observeEvent(input$word_answer, {

      shiny::req(word_to_translate())  # Prevent error when submit is pressed but no sentence has been sampled yet.

      real_translation <- .GlobalEnv$quiz_data$vocab_data  %>%
        dplyr::filter(!! rlang::sym(input$language_1) == word_to_translate()) %>%
        dplyr::pull(input$language_2)

      # alert("Correct submission = {input$guess %in% real_translation}") # there can be more than 1 correct translation, e.g. temps = time and weather

      if (input$word_answer %in% real_translation) {
        success("Correct submission")
        output$feedback <- shiny::renderText({"Correct!"})

        # Clear text input
        shiny::updateRadioButtons(session, "word_answer", selected = NULL)

      } else {
        # output$feedback <- shiny::renderText({
        #   alert("Incorrect submission")
        #   paste0("Incorrect. The actual translation is ", real_translation)
        # })
        alert("Incorrect submission")

        # output$word_display <- shiny::renderTable({
        #   alert("Show word to translate")
        #   data.frame(Word = word_to_translate())
        # }, width = "60%")
      }
    })
  })
}

