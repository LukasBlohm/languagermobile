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
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {

  # Get names of all .csv files in data_inputs/
  file_names <- dir("data_inputs", pattern = "\\.csv$")

  # Read the datasets
  purrr::walk(file_names, load_datasets)

  # Get names of the data frames in .GlobalEnv (which start with "df_")
  .GlobalEnv$v_df_names <- names(.GlobalEnv)[stringr::str_starts(
    names(.GlobalEnv), pattern = stringr::fixed("df_"))]

  # Get list of column names for these data frames
  .GlobalEnv$l_colnames <- purrr::map(v_df_names, ~ .GlobalEnv[[.x]] %>% colnames()) %>%
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
    golem_opts = list(...)
  )
}
