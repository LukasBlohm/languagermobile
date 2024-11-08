#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  translator = FALSE,
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {

  source("R/app_global.R")

  # Get names of all .csv files in data_inputs/
  file_names <- dir("data", pattern = "\\.csv$")

  # Read the datasets
  purrr::walk(file_names, load_datasets)

  # .GlobalEnv$token <- readRDS("token.rds")

  rlang::try_fetch(
    {
      # .GlobalEnv$df_dropbox <- rdrop2::drop_read_csv(
      #   .GlobalEnv$path_dropbox
      # )


      # rdrop2::drop_auth()
      # rdrop2::drop_auth(rdstoken = "token.rds")
      rdrop2::drop_download(
        .GlobalEnv$path_dropbox,
        verbose = FALSE,
        overwrite = TRUE,
        dtoken = .GlobalEnv$token
      )
      .GlobalEnv$df_dropbox <- readr::read_csv(
        .GlobalEnv$path_dropbox, show_col_types = FALSE
        )
      },
    error = \(cnd) {
      cli::cli_alert_warning("Dropbox token expired.")
      # rdrop2::drop_auth(new_user = TRUE, rdstoken = "token.rds")
      # token <- rdrop2::drop_auth()
      # saveRDS(token, file = "token.rds")
    }
  )


  # Get names of the data frames in .GlobalEnv (which start with "df_")
  .GlobalEnv$v_df_names <- names(.GlobalEnv)[stringr::str_starts(names(.GlobalEnv), pattern = stringr::fixed("df_"))]

  # Get list of column names for these data frames
  .GlobalEnv$l_colnames <- purrr::map(
    v_df_names, \(df) .GlobalEnv[[df]] %>% colnames()
    ) %>%
    purrr::set_names(v_df_names)


  .GlobalEnv$quiz_data <- shiny::reactiveValues(vocab_data = data.frame())

  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list()
  )
}


