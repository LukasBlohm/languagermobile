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

  explore_ui(ns)
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
        "language_selected",
        choices = get_languages(input),
        selected = "FR"
      )
    })

    shiny::observe({

      languages <- get_languages(input)
      other_options <- languages[languages != input$language_selected]

      shiny::updateSelectInput(
        session = session,
        "other_languages",
        choices = other_options,
        selected = other_options #[1]
      )
    })

    shiny::observe({
      shiny::req(paste0("df_", input$dataset))
      cli::cli_h2("Activate dataset {input$dataset}")

      df_name <- paste0("df_", input$dataset)

      df_active(.GlobalEnv[[df_name]])
    })

    list_reactives <- shiny::reactiveValues(
      show_translation = FALSE
    )

    # Update hidden text input to control appearance of the priority slider
    shiny::observe({
      has_priority <- "priority" %in% colnames(df_active()) & expression_original() != ""

      shiny::updateTextInput(
        session, "has_priority",
        value = ifelse(has_priority, "true", "false")
        )
    })


    shiny::observe({

      if (input$check_automode) {

        cli::cli_alert("Automatic sample")

        shiny::invalidateLater(1000 * shiny::isolate(input$sample_speed), session)

        if ("priority" %in% colnames(df_active())) {
          expression_original(
            dplyr::slice_sample(
              df_active(),
              weight_by = priority,
              n = 1
            ) %>%
              dplyr::pull(input$language_selected)
          )
        } else {
          expression_original(
            sample(x = dplyr::pull(df_active()[, input$language_selected]), size = 1)
          )
        }
      }
    })

    shiny::observeEvent(input$btn_load, {

      cli::cli_alert("Manual Sample")
      list_reactives$show_translation <- FALSE

      expression_original(
        sample(x = dplyr::pull(df_active()[, input$language_selected]), size = 1)
      )
    })


    shiny::observeEvent(input$btn_show_result, {
      list_reactives$show_translation <- TRUE
    })

    shiny::observeEvent(input$language_selected, {
      list_reactives$show_translation <- FALSE
      expression_original("")
    })

    shiny::observeEvent(input$dataset, {
      list_reactives$show_translation <- FALSE
      expression_original("")
    })

    shiny::observe({

      shiny::req(df_active())
      shiny::req(input$other_languages)

      if (list_reactives$show_translation || input$check_automode || input$check_autotranslate) {
        cli::cli_text("Show translation")

        expression_translated(
          df_active() %>%
            dplyr::filter(
              !! rlang::sym(input$language_selected) == expression_original()
            ) %>%
            dplyr::select(tidyselect::all_of(input$other_languages))
        )

        output$table <- shiny::renderTable({
          shiny::req(expression_original())  # Prevent error when show_translation is pressed but no sentence has been sampled yet.
          shiny::req(expression_translated())

          cli::cli_alert("Original expression: {expression_original()}")
          cli::cli_alert("Translation: {expression_translated()}")

          data.frame(Original = expression_original(),
                     Translation = expression_translated())
        }, width = "100%", align = "l")
      } else {
        cli::cli_alert_info("Hide translation")
        output$table <- shiny::renderTable({

          cli::cli_alert("Original expression: {expression_original()}")

          data.frame(Original = expression_original(),
                     Translation = "")
        }, width = "100%", align = "l")
      }
    })


    # new priority slider -----------------------------------------------------

    shiny::observe({

      shiny::req(expression_original())

      if (!input$check_automode && input$dataset %in% c("dropbox", "phone_notes")) {

        current_priority <- df_active() %>%
          dplyr::filter(!! rlang::sym(input$language_selected) == expression_original()) %>%
          dplyr::pull("priority")

        shiny::updateSliderInput(
          session = session,
          "priority",
          value = current_priority
        )
      }
    })

    shiny::observeEvent(input$priority, {

      shiny::req(df_active())

      df <- dplyr::mutate(
          df_active(),
          priority = dplyr::case_when(
            !! rlang::sym(input$language_selected) == expression_original() ~ input$priority,
            TRUE ~ priority
          )
        )

      readr::write_csv(df, .GlobalEnv$path_dropbox)
      rdrop2::drop_upload(
        .GlobalEnv$path_dropbox, mode = "overwrite", dtoken = .GlobalEnv$token
        )

      # Update df_active
      df_active(df)
    })
  })
}
