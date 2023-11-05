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
                         choices = c("EN", "FR"),
                         selected = "FR")
      ),
      column(width = 4,
             selectInput(ns("dataset"),
                         "Choose Dataset:",
                         choices = c("sentences", "idiom"),
                         selected = "sentences")
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

    # Read the sentence data
    df_sentences_base <- read.csv("sentences.csv")
    df_idioms <- read.csv("idioms.csv")

    df_active <- reactiveVal(data.frame())
    sentence_to_translate <- reactiveVal("")

    observe({
      if (input$dataset == "sentences") {
        df_active(df_sentences_base)
      } else if (input$dataset == "idioms") {
        df_active(df_idioms)
      }
    })

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

    # sentence_to_translate <- eventReactive(input$btn_load, {
    #   message("Sample a random sentence")
    #   # print(df_active()[, input$language_1])
    #   list_reactives$show_translation <- FALSE
    #   sample(x = df_active()[, input$language_1], size = 1)
    # })

    observeEvent(input$btn_load, {
      message("Sample a random sentence")
      list_reactives$show_translation <- FALSE
      sentence_to_translate(
        sample(x = df_active()[, input$language_1], size = 1)
        )
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
        sentence_translated <- df_active()[df_active()[[input$language_1]] == sentence_to_translate(),
                          list_reactives$other_language]

        message("Translation: ", sentence_translated)
        output$table <- renderTable({
          req(sentence_to_translate())  # Prevent error when show_translation is pressed but no sentence has been sampled yet.
          print(paste0("sentence to translate:", sentence_to_translate()))
          print(paste0("translation:", sentence_translated))
          data.frame(Original = sentence_to_translate(),
                     Translation = sentence_translated)
        },
        width = "60%", align = "c")
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
