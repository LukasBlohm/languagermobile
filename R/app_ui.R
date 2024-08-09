#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @noRd
app_ui <- function(request) {

  htmltools::tagList(
    # Leave this function for adding external resources
    # golem_add_external_resources()

    shinyMobile::f7Page(

      options = list(dark = FALSE, filled = FALSE, theme = "ios"),
      title = "languagermobile",
      shinyMobile::f7TabLayout(

        navbar = NULL,
        # navbar = shinyMobile::f7Navbar(
        #   title = "",
        #   hairline = FALSE,
        #   leftPanel = FALSE,
        #   rightPanel = FALSE
        # ),

        shinyMobile::f7Tabs(
          animated = TRUE,
          id = "tabs",
          shinyMobile::f7Tab(
            title = "Explorer",
            tabName = "Explorer",
            icon = shinyMobile::f7Icon("folder"),
            active = TRUE,
            mod_explore_ui("explore_module")
          ),
          shinyMobile::f7Tab(
            title = "Vocab",
            tabName = "Vocab",
            icon = shinyMobile::f7Icon("folder"),
            active = TRUE,
            mod_vocab_ui("vocab_module")
          )
        )
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
      app_title = "languagermobile"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
