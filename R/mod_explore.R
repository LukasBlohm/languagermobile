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

    # fluidRow(
    #   column(width = 4,
    #          selectInput(ns("dataset"),
    #                      "Choose Dataset:",
    #                      choices = NULL,
    #                      selected = NULL))
    # ),

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
    sentence_to_translate <- reactiveVal("")

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
      df_active(.GlobalEnv[[paste0("df_", input$dataset)]])
    })

    list_reactives <- reactiveValues(
      other_language = "EN",
      show_translation = FALSE
    )

    # observe({
    #   list_reactives$other_language <- input$language_2
    # })

    # observe({
    #   # print(input$language_1)
    #   if (input$language_1 == "FR") {
    #     message("Language 1 set to FR, language 2 set to EN")
    #     list_reactives$other_language <- "EN"
    #   } else {
    #     message("Language 1 set to EN, language 2 set to FR")
    #     list_reactives$other_language <- "FR"
    #   }
    # })


    observeEvent(input$btn_load, {
      message("Sample a random sentence")
      list_reactives$show_translation <- FALSE

      print("head(df_active())")
      print(head(df_active()))

      try(sentence_to_translate(
        sample(x = dplyr::pull(df_active()[, input$language_1]), size = 1)
      ))
    })

    observeEvent(input$btn_show_result, {
      list_reactives$show_translation <- TRUE
    })

    observeEvent(input$language_1, {
      list_reactives$show_translation <- FALSE
      sentence_to_translate("")
    })

    observeEvent(input$dataset, {
      list_reactives$show_translation <- FALSE
      sentence_to_translate("")
    })

    observe({
      if (list_reactives$show_translation) {
        message("Show translation")
        try(
          sentence_translated <-
            df_active()[df_active()[[input$language_1]] == sentence_to_translate(),
                        # list_reactives$other_language
                        input$language_2]
        )

        try(message("Translation: ", sentence_translated))

        try(
          output$table <- renderTable({
            req(sentence_to_translate())  # Prevent error when show_translation is pressed but no sentence has been sampled yet.
            print(paste0("sentence to translate:", sentence_to_translate()))
            print(paste0("translation:", sentence_translated))
            data.frame(Original = sentence_to_translate(),
                       Translation = sentence_translated)
          }, width = "60%", align = "c")
        )
      } else {
        message("Hide translation")
        output$table <- renderTable({
          print(paste0("sentence to translate:", sentence_to_translate()))
          data.frame(Original = sentence_to_translate(),
                     Translation = "")
        },
        width = "60%", align = "c")
      }
    })
  })
}
