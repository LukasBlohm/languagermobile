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
  ns <- NS(id)

  tagList(
    br(),
    fluidRow(
      column(width = 3,
             selectInput(ns("dataset"),
                         "Dataset",
                         choices = NULL,
                         selected = NULL
                         )
             ),
      column(width = 3,
             selectInput(ns("language_1"),
                         "Original Language",
                         choices = NULL,
                         selected = NULL
             )
      ),
      column(width = 3,
             selectInput(ns("language_2"),
                         "Translation",
                         choices = NULL,
                         selected = NULL
             )
      )
    ),

    br(),

    fluidRow(
      column(width = 4,
             actionButton(ns("btn_load"), "Sample new expression")
      ),
      column(width = 4,
             actionButton(ns("btn_show_result"), "Show translation")
      )
    ),

    # textOutput(ns("feedback")),
    tags$script(HTML(submit_on_enter(btn_id = ns("btn_show_result")))),
    tags$script(HTML(submit_on_enter(btn_id = ns("btn_load")))),

    br(),
    br(),
    tableOutput(ns("table")),

  )
}

#' explore Server Functions
#'
#' @noRd
mod_explore_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    df_active <- reactiveVal(tibble::tibble())
    expression_original <- reactiveVal("")

    shiny::updateSelectInput(
      session = session,
      "dataset",
      choices = generate_df_ui_labels()
    )

    observe({
      shiny::updateSelectInput(
        session = session,
        "language_1",
        choices = get_languages(input),
        selected = "FR"
      )
    })

    observe({

      languages <- get_languages(input)
      other_options <- languages[languages != input$language_1]

      shiny::updateSelectInput(
        session = session,
        "language_2",
        choices = other_options,
        selected = other_options[1]
      )
    })

    observe({
      req(paste0("df_", input$dataset))
      message(short_separator)
      message("Activate dataset ", input$dataset)
      df_active(.GlobalEnv[[paste0("df_", input$dataset)]])
    })

    list_reactives <- reactiveValues(
      show_translation = FALSE
    )

    observeEvent(input$btn_load, {
      message("Sample an expression")
      list_reactives$show_translation <- FALSE

      # print("head(df_active())")
      # print(head(df_active()))

      try(expression_original(
        sample(x = dplyr::pull(df_active()[, input$language_1]), size = 1)
      ))
    })

    observeEvent(input$btn_show_result, {
      list_reactives$show_translation <- TRUE
    })

    observeEvent(input$language_1, {
      list_reactives$show_translation <- FALSE
      expression_original("")
    })

    observeEvent(input$dataset, {
      list_reactives$show_translation <- FALSE
      expression_original("")
    })

    observe({
      if (list_reactives$show_translation) {
        message("Show translation")
        try(
          expression_translated <-
            df_active()[df_active()[[input$language_1]] == expression_original(),
                        input$language_2]
        )

        try(
          output$table <- renderTable({
            req(expression_original())  # Prevent error when show_translation is pressed but no sentence has been sampled yet.

            message("Original expression: ", expression_original())
            message("Translation: ", expression_translated)

            data.frame(Original = expression_original(),
                       Translation = expression_translated)
          }, width = "60%", align = "c")
        )
      } else {
        message("Hide translation")
        output$table <- renderTable({

          message("Original expression: ", expression_original())

          data.frame(Original = expression_original(),
                     Translation = "")
        },
        width = "100%", align = "c")
      }
    })
  })
}
