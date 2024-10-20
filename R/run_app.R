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
  file_names <- dir("data_inputs", pattern = "\\.csv$")

  # Read the datasets
  purrr::walk(file_names, load_datasets)

  rdrop2::drop_auth(rdstoken = "token.rds")
  .GlobalEnv$token <- readRDS("token.rds")
  rdrop2::drop_download(
    .GlobalEnv$path_dropbox,
    overwrite = TRUE,
    dtoken = .GlobalEnv$token
    )
  .GlobalEnv$df_dropbox <- readr::read_csv(.GlobalEnv$path_dropbox)

  # Get names of the data frames in .GlobalEnv (which start with "df_")
  .GlobalEnv$v_df_names <- names(.GlobalEnv)[stringr::str_starts(names(.GlobalEnv), pattern = stringr::fixed("df_"))]

  # Get list of column names for these data frames
  .GlobalEnv$l_colnames <- purrr::map(
    v_df_names, \(df) .GlobalEnv[[df]] %>% colnames()
    ) %>%
    purrr::set_names(v_df_names)


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


