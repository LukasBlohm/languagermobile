#' sentence UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sentence_ui <- function(id){
  ns <- NS(id)

  tagList(
    br(),
    fluidRow(
      column(width = 4,
             selectInput(ns("language_1"),
                         "Choose Language:",
                         choices = NULL,
                         selected = NULL
                         # choices = c("EN", "FR"),
                         # selected = "FR"
                         )
      ),
      column(width = 4,
             selectInput(ns("dataset"),
                         "Choose Dataset:",
                         choices = NULL,
                         selected = NULL
                         # choices = c("sentences", "idioms"),
                         # selected = "sentences"
                         )
             )
      ),

    br(),

    fluidRow(
      column(width = 4,
             actionButton(ns("btn_load"), "Show new sentence")
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

#' sentence Server Functions
#'
#' @noRd
mod_sentence_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Read the datasets
    # df_sentences <- read.csv("data_inputs/sentences.csv")
    # df_idioms <- read.csv("data_inputs/idioms.csv")
    # df_argot <- read.csv("data_inputs/argot.csv")

    # Get names of all .csv files in data_inputs/
    # file_names <- dir("data_inputs", pattern = "\\.csv$")

    # purrr::walk(paste0("data_inputs/", file_names[3:4]),
    #     ~ read.csv(.x) %>% assign(x = stringr::str_remove(file_names[.x], ".csv")),
    #     envir = .GlobalEnv)


    df_active <- reactiveVal(tibble::tibble())
    sentence_to_translate <- reactiveVal("")

    shiny::updateSelectInput(
      session = session,
      "dataset",
      choices = names(.GlobalEnv)[stringr::str_starts(
        names(.GlobalEnv), pattern = stringr::fixed("df_")
        )]
      )

    observe({
      shiny::updateSelectInput(
        session = session,
        "language_1",
        choices = l_colnames[[input$dataset]],
        selected = "FR"
      )
    })



    observe({

      req(input$dataset)
      df_active(.GlobalEnv[[input$dataset]])
    })

    # observe({
    #   if (input$dataset == "df_sentences") {
    #     df_active(.GlobalEnv$df_sentences)
    #   } else if (input$dataset == "df_idioms") {
    #     df_active(.GlobalEnv$df_idioms)
    #   } else if (input$dataset == "df_argot") {
    #     df_active(.GlobalEnv$df_argot)
    #   }
    # })

    list_reactives <- reactiveValues(
      other_language = "EN",
      show_translation = FALSE
    )

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


    observe({
      if (list_reactives$show_translation) {
        message("Show translation")
        try(
          sentence_translated <-
            df_active()[df_active()[[input$language_1]] == sentence_to_translate(),
                        list_reactives$other_language]
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
