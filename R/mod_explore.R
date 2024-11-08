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
    has_priority <- shiny::reactiveVal(NA)
    priority_initial <- shiny::reactiveVal(NA_integer_)
    priority_current <- shiny::reactiveVal(NA_integer_)
    df_sample_history <- shiny::reactiveVal(data.frame())

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

    shiny::observeEvent(input$dataset, {
      shiny::req(paste0("df_", input$dataset))
      cli::cli_h2("Activate dataset {input$dataset}")

      df_name <- paste0("df_", input$dataset)

      df_active(.GlobalEnv[[df_name]])

      has_priority("priority" %in% colnames(df_active()) & expression_original() != "")
    })

    list_reactives <- shiny::reactiveValues(
      show_translation = FALSE
    )

    # Update hidden text input to control appearance of the priority slider
    shiny::observe({
      shiny::updateTextInput(
        session, "has_priority",
        value = ifelse(has_priority(), "true", "false")
        )
    })


    shiny::observe({

      if (input$check_automode) {

        cli::cli_alert("Automatic sample")

        shiny::invalidateLater(1000 * shiny::isolate(input$sample_speed), session)

        v_expr_history <- rlang::try_fetch(
          dplyr::pull(shiny::isolate(df_sample_history()), "Original"),
          error = \(cnd) character()
        )

        df_to_sample <- df_active() %>%
          dplyr::filter(! (!!rlang::sym(input$language_selected)) %in% v_expr_history)

        if (has_priority()) {

          expression_original(
            dplyr::slice_sample(
              df_to_sample,
              weight_by = priority,
              n = 1
            ) %>%
              dplyr::pull(input$language_selected)
          )
        } else {
          expression_original(
            sample(x = dplyr::pull(df_to_sample[, input$language_selected]), size = 1)
          )
        }
      }
    })


    # input$btn_load ----------------------------------------------------------

    shiny::observeEvent(input$btn_load, {

      list_reactives$show_translation <- FALSE

      cli::cli_alert("priority_initial() old sample: {priority_initial()}")
      cli::cli_alert("priority_current() old sample: {priority_current()}")

      # Equal when priority_update is not NA
      if (has_priority() && isTRUE(priority_initial() != priority_current())) {
        cli::cli_alert_info("Save priority update")
        readr::write_csv(df_active(), .GlobalEnv$path_dropbox)
        suppressWarnings(rdrop2::drop_upload(
          .GlobalEnv$path_dropbox, mode = "overwrite",
          dtoken = .GlobalEnv$token
        ))
      }

      expression_original <- sample(
        x = dplyr::pull(df_active()[, input$language_selected]),
        size = 1
        )

      cli::cli_alert("Manual Sample: {expression_original}")

      expression_original(expression_original )

      if (has_priority()) {

        priority_initial <- df_active() %>%
          dplyr::filter(
            !! rlang::sym(input$language_selected) == expression_original
          ) %>%
          dplyr::pull("priority")


        cli::cli_alert("priority_initial() new sample: {priority_initial}")

        priority_initial(priority_initial)
      }
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


    # Translation handling ----------------------------------------------------

    shiny::observe({

      shiny::req(df_active())
      shiny::req(expression_original())
      shiny::req(input$other_languages)

      if (list_reactives$show_translation || input$check_automode || input$check_autotranslate) {

        if (!input$check_automode || !input$check_autotranslate) {
          cli::cli_text("Show translation")
        }

        expression_translated(
          df_active() %>%
            dplyr::filter(
              !! rlang::sym(input$language_selected) == expression_original()
            ) %>%
            dplyr::select(tidyselect::any_of(input$other_languages))
        )

        df_display_current <- data.frame(
          Original = expression_original(),
          Translation = expression_translated()
          )

        df_sample_history(
          dplyr::bind_rows(
            shiny::isolate(df_sample_history()), df_display_current
            ) %>%
            dplyr::distinct()
          )

        output$table <- shiny::renderTable({
          shiny::req(expression_original())  # Prevent error when show_translation is pressed but no sentence has been sampled yet.
          shiny::req(expression_translated())

          cli::cli_alert("Original expression: {expression_original()}")
          cli::cli_alert("Translation: {expression_translated()}")

          df_display_current

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

    output$table_history <- shiny::renderTable({
      shiny::req(df_sample_history())

      if (nrow(df_sample_history()) > 0) {
        df_sample_history()
      }


    }, width = "100%", align = "l")


    # update priority slider -----------------------------------------------------

    shiny::observeEvent(expression_original(), {

      shiny::req(expression_original())

      if (!input$check_automode && input$dataset %in% c("dropbox", "phone_notes")) {

        priority_current <- df_active() %>%
          dplyr::filter(!! rlang::sym(input$language_selected) == expression_original()) %>%
          dplyr::pull("priority")


        shiny::updateSliderInput(
          session = session,
          "priority",
          value = priority_current
        )

        priority_current(priority_current)
      }
    })


    # update priority in data -------------------------------------------------

    shiny::observeEvent(input$priority, {

      shiny::req(df_active())

      df <- dplyr::mutate(
          df_active(),
          priority = dplyr::case_when(
            !! rlang::sym(input$language_selected) == expression_original() ~ input$priority,
            TRUE ~ priority
          )
        )

      priority_current(input$priority)

      # Update df_active
      df_active(df)
    })

    shiny::observeEvent(df_sample_history(), {

      shiny::req(input$language_selected)
      shiny::req(input$other_languages)
      shiny::req(df_sample_history())

      if (nrow(df_sample_history()) > 1) {
        vocab_data <- df_sample_history() %>%
          dplyr::select(
            tidyselect::any_of(c("Original", input$language_selected, input$other_languages))
          )

        if (nrow(df_sample_history()) > 0) {
          colnames(vocab_data)[1] <- input$language_selected
        }

        .GlobalEnv$quiz_data$vocab_data <- vocab_data
        success("Update vocab data")
      }
    })

    shiny::observeEvent(input$clear_table_history, {
      info("Clear history data")
      df_sample_history(data.frame())
      .GlobalEnv$quiz_data$vocab_data <- df_dropbox_static
      info("colnames available: {colnames(.GlobalEnv$quiz_data$vocab_data)}")
    })
  })
}
