#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @import shiny
#' @noRd
app_ui <- function(request) {

  tagList(
    # Leave this function for adding external resources
    # golem_add_external_resources()
    fluidPage(
      theme = bslib::bs_theme(version = 5, bootswatch = "slate"),
      # h1("languager")

      tabsetPanel(
        id = "tabs",  # Set ID for conditional module loading

        tabPanel(
          title = "Explorer",
          mod_explore_ui("explore_module")
        ),
        tabPanel(
          title = "Vocab",
          mod_vocab_ui("vocab_module")
        ),

        if (golem::get_golem_options("translator")) {
          tabPanel(
            title = "Translator",
            mod_translate_ui("translate_module")
          )
        }
      )
    )
  )
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "languager"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
