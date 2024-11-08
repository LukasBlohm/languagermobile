#' explore_ui_bslib
#'
#' Create the UI for mod_explore with shiny functions.
#'
#' @return UI
#'
#' @noRd
explore_ui <- function(ns) {

  bslib::page(

    # htmltools::br(),

    shinyMobile::f7Card(
      outline = TRUE,
      raised = TRUE,
      divider = TRUE,
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::selectInput(
            ns("dataset"), "Dataset", choices = NULL
          )
        ),
        shiny::column(
          width = 3,
          shiny::selectInput(
            ns("language_selected"), "Original Language",
            choices = NULL,
          )
        ),
        shiny::column(
          width = 3,
          shiny::selectInput(
            ns("other_languages"),
            "Translation",
            choices = NULL,
            multiple = TRUE
          )
        )
      )
    ),


    shiny::conditionalPanel(
      condition = "!input.check_automode",
      ns = ns,
      shinyMobile::f7Card(
        outline = TRUE,
        raised = TRUE,
        divider = TRUE,
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shinyMobile::f7List(
              shinyMobile::f7Toggle(
                ns("check_autotranslate"), label = "Automatic translation",
                checked = TRUE,
                color = .GlobalEnv$main_color
              )
            )
          ),
          shiny::column(
            width = 6,
            conditionalPanel(
              condition = "!input.check_autotranslate",
              ns = ns,
              shiny::actionButton(
                ns("btn_show_result"), "Show translation",
                style='padding:5px; font-size:90%'
              )
            )
          )
        )#, height = "70px"
      )
    ),

    shinyMobile::f7Card(
      outline = TRUE,
      raised = TRUE,
      divider = TRUE,
      full_screen = TRUE,
      shiny::tableOutput(ns("table"))
    ),



    shinyMobile::f7Card(
      outline = TRUE,
      raised = TRUE,
      divider = TRUE,
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::conditionalPanel(
            condition = "!input.check_automode",
            ns = ns,
            shiny::actionButton(
              ns("btn_load"), "Sample new expression",
              style='padding:5px; font-size:90%'
            )
          ),
          shiny::conditionalPanel(
            condition = "input.check_automode",
            ns = ns,
            shiny::sliderInput(
              ns("sample_speed"), "Speed (sec)",
              value = 2, min = 1, max = 5, step = 0.5
            )
          )
        ),
        shiny::column(
          width = 6,
          shinyMobile::f7Toggle(
            # shinyMobile::f7Checkbox(
            ns("check_automode"), label = "Automatic Mode", checked = FALSE,
            color = .GlobalEnv$main_color
          )
        )
      ), height = ifelse("input.check_automode", "120px", "70px")
    ),


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


    # Hidden input to store has_priority
    tags$style(htmltools::HTML("#explore_module-has_priority {display: none;}")),
    shiny::textInput(ns("has_priority"), label = NULL, value = "false"),

    shiny::conditionalPanel(
      condition = "!input.check_automode && input.has_priority == 'true'",
      ns = ns,
      shinyMobile::f7Card(
        outline = TRUE,
        raised = TRUE,
        divider = TRUE,
        full_screen = TRUE,
        shiny::sliderInput(
          ns("priority"), "Priority",
          value = 4,
          min = 1, max = 5, step = 1
        )
      )
    ),

    shinyMobile::f7Card(
      outline = TRUE,
      raised = TRUE,
      divider = TRUE,
      full_screen = TRUE,
      shiny::tableOutput(ns("table_history"))
      ),

    shinyMobile::f7Card(
      outline = TRUE,
      raised = TRUE,
      divider = TRUE,
      full_screen = TRUE,
      shiny::actionButton(
        ns("clear_table_history"), "Clear history"
        )
    )
  )
}





