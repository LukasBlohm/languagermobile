#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @noRd
app_ui <- function(request) {

  # shinyWidgets::setSliderColor(.GlobalEnv$main_color_hex, 1)



  htmltools::tagList(
    # Leave this function for adding external resources
    # golem_add_external_resources()


    shinyMobile::f7Page(

      shinyWidgets::chooseSliderSkin(
        # skin = c("Shiny", "Flat", "Modern", "Nice", "Simple", "HTML5", "Round", "Square"),
        skin = "Flat",
        color = .GlobalEnv$main_color_hex
      ),

      allowPWA = TRUE,
      options = list(
        dark = FALSE,
        color = .GlobalEnv$main_color,
        filled = FALSE,
        theme = "ios"
        ),
      title = "languagermobile",
      shinyMobile::f7TabLayout(

        navbar = NULL,

        shinyMobile::f7Tabs(
          id = "tabs",
          animated = TRUE,
          style = "toolbar",
          shinyMobile::f7Tab(
            title = "Explorer",
            tabName = "Explorer",
            icon = shinyMobile::f7Icon("eye"),
            active = TRUE,
            mod_explore_ui("explore_module")
          ),
          shinyMobile::f7Tab(
            title = "Vocab",
            tabName = "Vocab",
            icon = shinyMobile::f7Icon("question"),
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
