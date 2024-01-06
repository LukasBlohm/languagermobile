#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_explore_ui <- function(id){

  ns <- shiny::NS(id)

  explore_ui_bslib(ns)
}





#' explore Server Functions
#'
#' @noRd
mod_explore_server <- function(id){

  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    df_active <- shiny::reactiveVal(tibble::tibble())
    expression_original <- shiny::reactiveVal("")
    expression_translated <- shiny::reactiveVal("")

    shiny::updateSelectInput(
      session = session,
      "dataset",
      choices = generate_df_ui_labels()
    )

    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        "language_1",
        choices = get_languages(input),
        selected = "FR"
      )
    })

    shiny::observe({

      languages <- get_languages(input)
      other_options <- languages[languages != input$language_1]

      shiny::updateSelectInput(
        session = session,
        "language_2",
        choices = other_options,
        selected = other_options[1]
      )
    })

    shiny::observe({
      shiny::req(paste0("df_", input$dataset))
      message(short_separator)
      message("Activate dataset ", input$dataset)
      df_active(.GlobalEnv[[paste0("df_", input$dataset)]])
    })

    list_reactives <- reactiveValues(
      show_translation = FALSE
    )

    shiny::observe({

      if (input$check_automode) {

        message("Automatic sample")

        shiny::invalidateLater(1000 * shiny::isolate(input$sample_speed), session)

        try(expression_original(
          sample(x = dplyr::pull(df_active()[, input$language_1]), size = 1)
        ))

      }
    })

    shiny::observeEvent(input$btn_load, {

      message("Manual Sample")
      list_reactives$show_translation <- FALSE

      try(expression_original(
        sample(x = dplyr::pull(df_active()[, input$language_1]), size = 1)
      ))
    })


    shiny::observeEvent(input$btn_show_result, {
      list_reactives$show_translation <- TRUE
    })

    shiny::observeEvent(input$language_1, {
      list_reactives$show_translation <- FALSE
      expression_original("")
    })

    shiny::observeEvent(input$dataset, {
      list_reactives$show_translation <- FALSE
      expression_original("")
    })

    shiny::observe({

      if (list_reactives$show_translation || input$check_automode || input$check_autotranslate) {
        message("Show translation")
        try(
          expression_translated(
            df_active()[df_active()[[input$language_1]] == expression_original(),
                        input$language_2]
          )
        )

        try(
          output$table <- shiny::renderTable({
            shiny::req(expression_original())  # Prevent error when show_translation is pressed but no sentence has been sampled yet.
            shiny::req(expression_translated())

            message("Original expression: ", expression_original())
            message("Translation: ", expression_translated())

            data.frame(Original = expression_original(),
                       Translation = expression_translated())
          }, width = "100%", align = "l")
        )
      } else {
        message("Hide translation")
        output$table <- shiny::renderTable({

          message("Original expression: ", expression_original())

          data.frame(Original = expression_original(),
                     Translation = "")
        }, width = "100%", align = "l")
      }
    })
  })
}
