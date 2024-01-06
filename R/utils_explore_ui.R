#' explore_ui_bslib
#'
#' Deprecated function to create the UI for mod_explore with shiny functions.
#'
#' @return UI
#'
#' @noRd
explore_ui_bslib <- function(ns) {

  bslib::page(

    htmltools::br(),

    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::selectInput(ns("dataset"),
                           "Dataset",
                           choices = NULL,
                           selected = NULL
        )
      ),
      shiny::column(
        width = 3,
        shiny::selectInput(ns("language_1"),
                           "Original Language",
                           choices = NULL,
                           selected = NULL
        )
      ),
      shiny::column(
        width = 3,
        shiny::selectInput(ns("language_2"),
                           "Translation",
                           choices = NULL,
                           selected = NULL
        )
      )
    ),

    htmltools::br(),

    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::checkboxInput(ns("check_autosample"), "Automatic Sampling", value = FALSE)
      ),
      shiny::column(
        width = 3,
        shiny::checkboxInput(ns("check_autotranslate"), "Automatic translation")
      )
    ),

    htmltools::br(),


    bslib::card(
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::actionButton(ns("btn_load"), "Sample new expression")
        ),
        shiny::column(
          width = 6,
          shiny::actionButton(ns("btn_show_result"), "Show translation")
          )
        )
      ),

    # textOutput(ns("feedback")),
    tags$script(htmltools::HTML(submit_on_enter(btn_id = ns("btn_show_result")))),
    tags$script(htmltools::HTML(submit_on_enter(btn_id = ns("btn_load")))),

    # htmltools::br(),
    # htmltools::br(),

    tags$head(
      tags$style(htmltools::HTML("
      .shiny-table td {
        width: 50%;  /* Adjust the percentage based on the number of columns */
      }
    "))
    ),

    bslib::card(
      full_screen = TRUE,
      shiny::tableOutput(ns("table")))
  )

}


#' explore_ui_shiny
#'
#' Deprecated function to create the UI for mod_explore with shiny functions.
#'
#' @return UI
#'
#' @noRd
explore_ui_shiny <- function(ns) {

  htmltools::tagList(
    htmltools::br(),
    shiny::fluidRow(
      shiny::column(width = 3,
                    shiny::selectInput(ns("dataset"),
                         "Dataset",
                         choices = NULL,
                         selected = NULL
             )
      ),
      shiny::column(width = 3,
             shiny::selectInput(ns("language_1"),
                         "Original Language",
                         choices = NULL,
                         selected = NULL
             )
      ),
      shiny::column(width = 3,
                    shiny::selectInput(ns("language_2"),
                         "Translation",
                         choices = NULL,
                         selected = NULL
                         )
                    )
    ),

    htmltools::br(),

    shiny::fluidRow(
      shiny::column(width = 4,
                    shiny::actionButton(ns("btn_load"), "Sample new expression")
      ),
      shiny::column(width = 4,
                    shiny::actionButton(ns("btn_show_result"), "Show translation")
      )
    ),

    # textOutput(ns("feedback")),
    tags$script(htmltools::HTML(submit_on_enter(btn_id = ns("btn_show_result")))),
    tags$script(htmltools::HTML(submit_on_enter(btn_id = ns("btn_load")))),

    htmltools::br(),
    htmltools::br(),
    shiny::tableOutput(ns("table")),

  )
}



